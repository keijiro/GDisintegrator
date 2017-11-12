// Geometry disintegrator effect
// https://github.com/keijiro/GDisintegrator

#include "Common.cginc"
#include "UnityGBuffer.cginc"
#include "UnityStandardUtils.cginc"
#include "SimplexNoise2D.hlsl"

// Cube map shadow caster; Used to render point light shadows on platforms
// that lacks depth cube map support.
#if defined(SHADOWS_CUBE) && !defined(SHADOWS_CUBE_IN_DEPTH_TEX)
#define PASS_CUBE_SHADOWCASTER
#endif

// Base properties
half4 _Color;
sampler2D _MainTex;
float4 _MainTex_ST;
half _Glossiness;
half _Metallic;

// Effect properties
half4 _Color2;
half _Glossiness2;
half _Metallic2;

// Dynamic properties
float4 _EffectVector;

// Vertex input attributes
struct Attributes
{
    float4 position : POSITION;
    float3 normal : NORMAL;
    float2 texcoord : TEXCOORD;
};

// Fragment varyings
struct Varyings
{
    float4 position : SV_POSITION;

#if defined(PASS_CUBE_SHADOWCASTER)
    // Cube map shadow caster pass
    float3 shadow : TEXCOORD0;

#elif defined(UNITY_PASS_SHADOWCASTER)
    // Default shadow caster pass

#else
    // GBuffer construction pass
    half3 normal : NORMAL;
    float2 texcoord : TEXCOORD0;
    float3 worldPos : TEXCOORD1;
    half4 ambient_ch : TEXCOORD2; // Ambient SH (xyz), channel select (w)

#endif
};

//
// Vertex stage
//

Attributes Vertex(Attributes input)
{
    // Only do object space to world space transform.
    input.position = mul(unity_ObjectToWorld, input.position);
    input.normal = UnityObjectToWorldNormal(input.normal);
    return input;
}

//
// Geometry stage
//

#define POINTS_PER_RING 24

Varyings VertexOutput(float3 wpos, half3 wnrm, float2 uv, half channel = 0)
{
    Varyings o;

#if defined(PASS_CUBE_SHADOWCASTER)
    // Cube map shadow caster pass: Transfer the shadow vector.
    o.position = UnityWorldToClipPos(float4(wpos, 1));
    o.shadow = wpos - _LightPositionRange.xyz;

#elif defined(UNITY_PASS_SHADOWCASTER)
    // Default shadow caster pass: Apply the shadow bias.
    float scos = dot(wnrm, normalize(UnityWorldSpaceLightDir(wpos)));
    wpos -= wnrm * unity_LightShadowBias.z * sqrt(1 - scos * scos);
    o.position = UnityApplyLinearShadowBias(UnityWorldToClipPos(float4(wpos, 1)));

#else
    // GBuffer construction pass
    o.position = UnityWorldToClipPos(float4(wpos, 1));
    o.normal = wnrm;
    o.texcoord = uv;
    o.worldPos = wpos;
    o.ambient_ch = half4(ShadeSHPerVertex(wnrm, 0), channel);

#endif
    return o;
}

float3 RingPoint(float3 tx, float3 ty, float phi, float2 np)
{
    return (cos(phi) * tx + sin(phi) * ty) * (1 + snoise(np) * 0.2);
}

[maxvertexcount(POINTS_PER_RING * 2)]
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

    float2 uv0 = input[0].texcoord;
    float2 uv1 = input[1].texcoord;
    float2 uv2 = input[2].texcoord;

    float3 center = (p0 + p1 + p2) / 3;

    // Deformation parameter
    float param = 1 - dot(_EffectVector.xyz, center) + _EffectVector.w;

    // Pass through the vertices if deformation hasn't been started yet.
    if (param < 0)
    {
        outStream.Append(VertexOutput(p0, n0, uv0));
        outStream.Append(VertexOutput(p1, n1, uv1));
        outStream.Append(VertexOutput(p2, n2, uv2));
        outStream.RestartStrip();
        return;
    }

    // Draw nothing at the end of deformation.
    if (param >= 1) return;

    // Choose ring/triangle randomly.
    uint seed = pid * 877;
    if (Random(seed) < 0.05)
    {
        // Construct the tangent space
        float3 tx = normalize(n0 + n1 + n2);
        float3 ty = normalize(cross(RandomVector(seed + 1), tx));
        float3 tz = normalize(cross(tx, ty));

        // Ring width
        float wid = 0.01;
        wid *= smoothstep(0, 0.2, param);
        wid *= smoothstep(0, 0.8, 1 - param);

        // Ring radius
        float rad = (1 + Random(seed + 4)) * 0.1;
        rad *= 1 - (1 - param) * (1 - param);

        // Noise offset
        float noffs = Random(seed + 5) * 3234.21 + param * 2;

        // Base angle
        float phi = Random(seed + 6) * UNITY_PI * 2;

        // Loop parameters
        float phi_di = UNITY_PI / POINTS_PER_RING;
        float np_di = 0.05 + 0.2 * Random(seed + 7);

        for (uint i = 0; i < POINTS_PER_RING; i++)
        {
            // Calculate three points to derive the gradient.
            float3 p0 = RingPoint(tx, ty, phi + phi_di * (i - 0.1), float2(noffs, np_di * (i - 0.1)));
            float3 p1 = RingPoint(tx, ty, phi + phi_di * (i      ), float2(noffs, np_di * (i      )));
            float3 p2 = RingPoint(tx, ty, phi + phi_di * (i + 0.1), float2(noffs, np_di * (i - 0.1)));

            // Position/normal
            float3 pos = center + p1 * rad;
            float3 nrm = normalize(cross(p2 - p0, tz));

            // Ring width curve
            float dz = wid * smoothstep(0, 0.8, 1 - abs(1 - i / 12.0));

            // Vertex outputs
            outStream.Append(VertexOutput(pos + tz * dz, nrm, 0, 1));
            outStream.Append(VertexOutput(pos - tz * dz, nrm, 0, 1));
        }

        outStream.RestartStrip();
    }
    else
    {
        // -- Triangle fx --
        // Simple scattering animation

        // We use smoothstep to make naturally damped linear motion.
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

        // Vertex outputs
        outStream.Append(VertexOutput(t_p0, normal, uv0));
        outStream.Append(VertexOutput(t_p1, normal, uv1));
        outStream.Append(VertexOutput(t_p2, normal, uv2));
        outStream.RestartStrip();
    }
}

//
// Fragment phase
//

#if defined(PASS_CUBE_SHADOWCASTER)

// Cube map shadow caster pass
half4 Fragment(Varyings input) : SV_Target
{
    float depth = length(input.shadow) + unity_LightShadowBias.x;
    return UnityEncodeCubeShadowDepth(depth * _LightPositionRange.w);
}

#elif defined(UNITY_PASS_SHADOWCASTER)

// Default shadow caster pass
half4 Fragment() : SV_Target { return 0; }

#else

// GBuffer construction pass
void Fragment(
    Varyings input,
    float vface : VFACE,
    out half4 outGBuffer0 : SV_Target0,
    out half4 outGBuffer1 : SV_Target1,
    out half4 outGBuffer2 : SV_Target2,
    out half4 outEmission : SV_Target3
)
{
    half3 albedo = tex2D(_MainTex, input.texcoord).rgb * _Color.rgb;

    // PBS workflow conversion (metallic -> specular)
    half3 c1_diff, c1_spec, c2_diff, c2_spec;
    half not_in_use;

    c1_diff = DiffuseAndSpecularFromMetallic(
        albedo, _Metallic,       // input
        c1_spec, not_in_use      // output
    );

    c2_diff = DiffuseAndSpecularFromMetallic(
        _Color2.rgb, _Metallic2, // input
        c2_spec, not_in_use      // output
    );

    // Update the GBuffer.
    UnityStandardData data;
    float ch = input.ambient_ch.w;
    data.diffuseColor = lerp(c1_diff, c2_diff, ch);
    data.occlusion = 1;
    data.specularColor = lerp(c1_spec, c2_spec, ch);
    data.smoothness = lerp(_Glossiness, _Glossiness2, ch);
    data.normalWorld = (vface < 0 ? -1 : 1) * input.normal;
    UnityStandardDataToGbuffer(data, outGBuffer0, outGBuffer1, outGBuffer2);

    // Output ambient light and edge emission to the emission buffer.
    half3 sh = ShadeSHPerPixel(data.normalWorld, input.ambient_ch.rgb, input.worldPos);
    outEmission = half4(sh * data.diffuseColor, 1);
}

#endif
