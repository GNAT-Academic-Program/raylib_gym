#include "raylib.h"
#include <stdio.h>

int check_hit(Model *model, Ray *ray)
{

   Mesh mesh = model->meshes[0];
   bool indexed = (mesh.indices != NULL);
   float *vertices = mesh.vertices;
   unsigned short *indices = mesh.indices;

   for (int i = 0; i < mesh.triangleCount; i++)
   {
      Vector3 v0, v1, v2;

      if (indexed)
      {
         // If indexed, use the indices to get vertex positions
         int idx0 = indices[i * 3 + 0];
         int idx1 = indices[i * 3 + 1];
         int idx2 = indices[i * 3 + 2];

         v0 = (Vector3){vertices[idx0 * 3 + 0], vertices[idx0 * 3 + 1], vertices[idx0 * 3 + 2]};
         v1 = (Vector3){vertices[idx1 * 3 + 0], vertices[idx1 * 3 + 1], vertices[idx1 * 3 + 2]};
         v2 = (Vector3){vertices[idx2 * 3 + 0], vertices[idx2 * 3 + 1], vertices[idx2 * 3 + 2]};
      }
      else
      {
         // If not indexed, vertices are in sequential order
         v0 = (Vector3){vertices[i * 9 + 0], vertices[i * 9 + 1], vertices[i * 9 + 2]};
         v1 = (Vector3){vertices[i * 9 + 3], vertices[i * 9 + 4], vertices[i * 9 + 5]};
         v2 = (Vector3){vertices[i * 9 + 6], vertices[i * 9 + 7], vertices[i * 9 + 8]};
      }

      RayCollision meshHitInfo = GetRayCollisionTriangle(*ray, v0, v1, v2);
      if (meshHitInfo.hit)
      {
         return meshHitInfo.distance;
      }
   }

   return 0;

   // printf("Model: %d", model->meshes[0].triangleCount);
   // printf("Ray: %f", ray->position.x);

   // RayCollision meshHitInfo = { 0 };
   // for (int m = 0; m < model->meshCount; m++)
   // {
   //     meshHitInfo = GetRayCollisionMesh(*ray, model->meshes[m], model->transform);

   //     printf("MeshHitInfo: %f", meshHitInfo.distance);

   //     if (meshHitInfo.hit)
   //     {
   //         return true;
   //     }
   // }
   // return false;
}