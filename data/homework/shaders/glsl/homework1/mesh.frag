#version 450

layout (set = 1, binding = 0) uniform sampler2D samplerColorMap;
layout (set = 2, binding = 0) uniform sampler2D samplerNormalMap;
layout (set = 3, binding = 0) uniform sampler2D samplerMRMap;
layout (set = 4, binding = 0) uniform sampler2D samplerEmissiveMap;

layout (location = 0) in vec3 inNormal;
layout (location = 1) in vec3 inColor;
layout (location = 2) in vec2 inUV;
layout (location = 3) in vec3 inViewVec;
layout (location = 4) in vec3 inLightVec;
layout (location = 5) in vec4 inTangent;

layout (location = 0) out vec4 outFragColor;

#define PI 3.1415926535897932384626433832795

vec3 calculateNormal()
{
	vec3 tangentNormal = texture(samplerNormalMap, inUV).xyz * 2.0 - 1.0;

	vec3 N = normalize(inNormal);
	vec3 T = normalize(inTangent.xyz);
	vec3 B = normalize(cross(N, T));
	mat3 TBN = mat3(T, B, N);
	return normalize(TBN * tangentNormal);
}

vec3 materialcolor()
{
	return texture(samplerColorMap, inUV).rgb * inColor;
}

// Normal Distribution function --------------------------------------
float D_GGX(float dotNH, float roughness)
{
	float alpha = roughness * roughness;
	float alpha2 = alpha * alpha;
	float denom = dotNH * dotNH * (alpha2 - 1.0) + 1.0;
	return (alpha2)/(PI * denom*denom); 
}

// Geometric Shadowing function --------------------------------------
float G_SchlicksmithGGX(float dotNL, float dotNV, float roughness)
{
	float r = (roughness + 1.0);
	float k = (r*r) / 8.0;
	float GL = dotNL / (dotNL * (1.0 - k) + k);
	float GV = dotNV / (dotNV * (1.0 - k) + k);
	return GL * GV;
}

// Fresnel function ----------------------------------------------------
vec3 F_Schlick(float cosTheta, float metallic)
{
	vec3 F0 = mix(vec3(0.04), materialcolor(), metallic); // * material.specular
	vec3 F = F0 + (1.0 - F0) * pow(1.0 - cosTheta, 5.0); 
	return F;    
}
vec3 F_SchlickR(float cosTheta, vec3 F0, float roughness)
{
	return F0 + (max(vec3(1.0 - roughness), F0) - F0) * pow(1.0 - cosTheta, 5.0);
}

// Specular BRDF composition --------------------------------------------

vec3 BRDF(vec3 L, vec3 V, vec3 N, float metallic, float roughness)
{
	// Precalculate vectors and dot products	
	vec3 H = normalize (V + L);
	float dotNV = clamp(dot(N, V), 0.0, 1.0);
	float dotNL = clamp(dot(N, L), 0.0, 1.0);
	float dotNH = clamp(dot(N, H), 0.0, 1.0);

	// Light color fixed
	vec3 lightColor = vec3(1.0);

	vec3 color = vec3(0.0);

	if (dotNL > 0.0)
	{
		float rroughness = max(0.05, roughness);
		// D = Normal distribution (Distribution of the microfacets)
		float D = D_GGX(dotNH, roughness); 
		// G = Geometric shadowing term (Microfacets shadowing)
		float G = G_SchlicksmithGGX(dotNL, dotNV, rroughness);
		// F = Fresnel factor (Reflectance depending on angle of incidence)
		vec3 F = F_Schlick(dotNV, metallic);

		vec3 spec = D * F * G / (4.0 * dotNL * dotNV + 0.001);

		vec3 kD = (vec3(1.0) - F) * (1.0 - metallic);

		color += (kD * materialcolor() / PI + spec) * dotNL;
		color = spec * dotNL * lightColor;
	}

	return color;
}



void main() 
{
	vec3 N = calculateNormal();
	vec3 V = normalize(inViewVec);

	vec3 orm = texture(samplerMRMap, inUV).rgb;
	float ao = orm.r;
	float metallic = orm.b;
	float roughness = orm.g;
	
	vec3 L = normalize(inLightVec);

	vec3 color = BRDF(L, V, N, metallic, roughness);

	// diffuse
	vec3 F0 = mix(vec3(0.04), materialcolor(), metallic);
	vec3 F = F_SchlickR(max(dot(N, V), 0.0), F0, roughness);

	// Ambient part
	vec3 kD = vec3(1.0) - F;
	kD *= (1.0 - metallic);
	vec3 ambient = kD * materialcolor() * texture(samplerMRMap, inUV).rrr;

	color += ambient;

	// gamma correction
	color = pow(color, vec3(1.0/2.2));
	
	outFragColor = vec4(color, 1.0) + texture(samplerEmissiveMap, inUV);

	// vec3 N = calculateNormal();
	// vec3 L = normalize(inLightVec);
	// vec3 V = normalize(inViewVec);
	// vec3 R = reflect(L, N);
	// vec3 diffuse = max(dot(N, L), 0.15) * inColor;
	// vec3 specular = pow(max(dot(R, V), 0.0), 16.0) * vec3(0.75);
	// outFragColor = vec4(diffuse * color.rgb + specular, 1.0);
	// outFragColor += texture(samplerEmissiveMap, inUV);		
}