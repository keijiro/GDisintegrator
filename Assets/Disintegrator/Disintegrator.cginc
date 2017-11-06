#include "Common.cginc"
#include "UnityGBuffer.cginc"
#include "SimplexNoise3D.hlsl"

// Base properties
half4 _Color;
half3 _SpecColor;
half _Glossiness;

// Effect properties
half4 _Color2;
half3 _SpecColor2;
half _Glossiness2;

// Edge properties
half3 _EdgeColor;

// Dynamic properties
//float4 _EffectVector;
float4 _EffectVector;

// Vertex input attributes
struct Attributes
{
    float4 position : POSITION;
    float3 normal : NORMAL;
};

// Fragment varyings
struct Varyings
{
    float4 position : SV_POSITION;
    float3 normal: NORMAL;
};

// Vertex stage
Attributes Vertex(Attributes input)
{
    input.position = mul(unity_ObjectToWorld, input.position);
    input.normal = UnityObjectToWorldNormal(input.normal);
    return input;
}

// Geometry stage
Varyings GeoOutWPosNrm(float3 wp, half3 wn)
{
    Varyings o;
    o.position = UnityWorldToClipPos(float4(wp, 1));
    o.normal = wn;
    return o;
}

[maxvertexcount(24)]
void Geometry(
    triangle Attributes input[3], uint pid : SV_PrimitiveID,
    inout TriangleStream<Varyings> outStream
)
{
    // Input vertices
    float3 p0 = input[0].position.xyz;
    float3 p1 = input[1].position.xyz;
    float3 p2 = input[2].position.xyz;

    float3 n0 = input[0].normal;
    float3 n1 = input[1].normal;
    float3 n2 = input[2].normal;

    float3 center = (p0 + p1 + p2) / 3;

    // Deformation parameter
    float param = 1 - dot(_EffectVector.xyz, center) + _EffectVector.w;

    // Pass through the vertices if deformation hasn't been started yet.
    if (param < 0)
    {
        outStream.Append(GeoOutWPosNrm(p0, n0));
        outStream.Append(GeoOutWPosNrm(p1, n1));
        outStream.Append(GeoOutWPosNrm(p2, n2));
        outStream.RestartStrip();
        return;
    }

    // Draw nothing at the end of deformation.
    if (param >= 1) return;

    // Choose cube/triangle randomly.
    uint seed = pid * 877;
    if (Random(seed) < 0.08)
    {
        float3 ay = float3(0, 1, 0);
        float3 az = normalize(n0 + n1 + n2);
        float3 ax = normalize(cross(ay, az));
        ay = normalize(cross(az, ax));

        float ss = smoothstep(0, 0.2, param) * smoothstep(0, 0.8, 1 - param);

        float rnd = Random(seed + 900);
        for (uint i = 0; i < 24; i++)
        {
            float4 sns = snoise_grad(float3(rnd * 3234.21, i * 0.084 + param*2, param));
            float phi = UNITY_PI * i / 24 + Random(seed + 2) * 6 + 0*Random(seed + 3) * 8 * param;
            float3 n = cos(phi) * ax + sin(phi) * az + ay * 0.3 * ss * (i & 1) * Random(seed + 5);
            outStream.Append(GeoOutWPosNrm(center + n * 0.4 * (param + 0.1) * (1 + sns.w * 0.8) * Random(seed + 4), n));
        }

        outStream.RestartStrip();
    }
    else
    {
        // -- Triangle fx --
        // Simple scattering animation

        // We use smoothstep to make naturally damped linear motion.
        // Q. Why don't you use 1-pow(1-param,2)?
        // A. Smoothstep is cooler than it. Forget Newtonian physics.
        float ss_param = smoothstep(0, 1, param);

        // Random motion
        float3 move = RandomVector(seed + 1) * ss_param * 0.5;

        // Random rotation
        float3 rot_angles = (RandomVector01(seed + 1) - 0.5) * 100;
        float3x3 rot_m = Euler3x3(rot_angles * ss_param);

        // Simple shrink
        float scale = 1 - ss_param;

        // Apply the animation.
        float3 t_p0 = mul(rot_m, p0 - center) * scale + center + move;
        float3 t_p1 = mul(rot_m, p1 - center) * scale + center + move;
        float3 t_p2 = mul(rot_m, p2 - center) * scale + center + move;
        float3 normal = normalize(cross(t_p1 - t_p0, t_p2 - t_p0));

        // Edge color (emission power) animation
        float edge = smoothstep(0, 0.1, param); // ease-in
        edge *= 1 + 20 * smoothstep(0, 0.1, 0.1 - param); // peak -> release

        // Vertex outputs (front face)
        outStream.Append(GeoOutWPosNrm(t_p0, normal));
        outStream.Append(GeoOutWPosNrm(t_p1, normal));
        outStream.Append(GeoOutWPosNrm(t_p2, normal));
        outStream.RestartStrip();

        // Vertex outputs (back face)
        outStream.Append(GeoOutWPosNrm(t_p0, -normal));
        outStream.Append(GeoOutWPosNrm(t_p2, -normal));
        outStream.Append(GeoOutWPosNrm(t_p1, -normal));
        outStream.RestartStrip();
    }
}

// Fragment phase
#ifdef VOXELIZER_SHADOW_CASTER

half4 Fragment() : SV_Target { return 0; }

#else

void Fragment(
    Varyings input, float vface : VFACE,
    out half4 outGBuffer0 : SV_Target0,
    out half4 outGBuffer1 : SV_Target1,
    out half4 outGBuffer2 : SV_Target2,
    out half4 outEmission : SV_Target3
)
{
    // Output to GBuffers.
    UnityStandardData data;
    data.diffuseColor = _Color.rgb;
    data.occlusion = 1;
    data.specularColor = _SpecColor;
    data.smoothness = _Glossiness;
    data.normalWorld = (vface < 0 ? -1 : 1) * input.normal;
    UnityStandardDataToGbuffer(data, outGBuffer0, outGBuffer1, outGBuffer2);

    outEmission = 0;
}

#endif
