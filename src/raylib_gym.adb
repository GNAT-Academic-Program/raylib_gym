with Raylib;
with Interfaces.C;            use Interfaces.C;
with Interfaces.C.Strings;    use Interfaces.C.Strings;
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with System;
with System.Address_To_Access_Conversions;
with System.Storage_Elements; use System.Storage_Elements;

procedure Raylib_Gym is

   subtype CFloat is Interfaces.C.C_float;

   function Lerp (start, stop, alpha : CFloat) return CFloat is
      (start + (stop - start) * alpha);

   DEG2RAD : constant CFloat := 3.14159265359 / 180.0;

   screenWidth  : constant := 800;
   screenHeight : constant := 450;
   Cam : aliased Raylib.Camera3D;

   Car : Raylib.Model;
   Race_Track : Raylib.Model;

   Outer_Boundary : Raylib.Model;
   Inner_Boundary : Raylib.Model;

   World_Pos      : constant Raylib.Vector3 := (0.0, 0.0, 0.0);
   World_Rot_Axis : constant Raylib.Vector3 := (0.0, 1.0, 0.0);
   World_Scale    : constant Raylib.Vector3 := (0.2, 0.2, 0.2);

   Car_Rot_Angle : CFloat := 0.0;

   -- New car variables
   Car_Position  : Raylib.Vector3 := (0.0, 0.0, 0.0);
   Car_Speed     : CFloat := 0.2;
   Car_Rotation  : CFloat := 0.0;
   Car_Forward   : Raylib.Vector3 := (0.0, 0.0, 1.0);

   package Mesh_Ptr_Conv is new System.Address_To_Access_Conversions (Raylib.Mesh);

   -- Instantiate generic math functions for CFloat
   package CFloat_Math is new Ada.Numerics.Generic_Elementary_Functions (CFloat);
   use CFloat_Math;

   function Vector3Distance (v1, v2 : Raylib.Vector3) return CFloat is
   begin
      return Sqrt ((v2.x - v1.x)**2 + (v2.y - v1.y)**2 + (v2.z - v1.z)**2);
   end Vector3Distance;

   function Vector3Add (v1, v2 : Raylib.Vector3) return Raylib.Vector3 is
   begin
      return (x => v1.x + v2.x,
              y => v1.y + v2.y,
              z => v1.z + v2.z);
   end Vector3Add;

   function Vector3Scale (v : Raylib.Vector3; scalar : CFloat) return Raylib.Vector3 is
   begin
      return (x => v.x * scalar,
              y => v.y * scalar,
              z => v.z * scalar);
   end Vector3Scale;

begin
   Raylib.InitWindow (screenWidth, screenHeight, New_String ("Raylib Gym"));

   Car            := Raylib.LoadModel (New_String ("./data/race_car.gltf"));
   Race_Track     := Raylib.LoadModel (New_String ("./data/race_track.gltf"));
   Outer_Boundary := Raylib.LoadModel (New_String ("./data/outer_boundary.gltf"));
   Inner_Boundary := Raylib.LoadModel (New_String ("./data/inner_boundary.gltf"));

   Cam.position    := (10.0, 10.0, 10.0);
   Cam.target      := (0.0, 0.0, 0.0);
   Cam.up          := (0.0, 1.0, 0.0);
   Cam.fovy        := 45.0;
   Cam.projection  := Raylib.CAMERA_PERSPECTIVE;

   Raylib.SetTargetFPS (60);
   Car_Position := (0.0, 0.0, 0.0);

   while not Raylib.WindowShouldClose loop
      declare
         Dt                 : CFloat := Raylib.GetFrameTime;
         Previous_Position  : constant Raylib.Vector3 := Car_Position;
         Collision_Hit      : Boolean := False;
         Collision_Distance : CFloat := 0.0;
      begin

         Raylib.BeginDrawing;

         Raylib.ClearBackground (Raylib.RAYWHITE);

         Raylib.BeginMode3D (Cam);

         -- Draw the track models
         Raylib.DrawModelEx (Outer_Boundary, World_Pos, World_Rot_Axis, 0.0,
                             World_Scale, Raylib.GRAY);
         Raylib.DrawModelEx (Inner_Boundary, World_Pos, World_Rot_Axis, 0.0,
                             World_Scale, Raylib.GRAY);

         -- Car movement controls
         if Raylib.IsKeyDown (Raylib.KEY_G) then
            Car_Rotation := Car_Rotation + 90.0 * Dt;
         end if;

         if Raylib.IsKeyDown (Raylib.KEY_J) then
            Car_Rotation := Car_Rotation - 90.0 * Dt;
         end if;

         -- Calculate forward direction
         Car_Forward.x := Sin (Car_Rotation * DEG2RAD);
         Car_Forward.z := Cos (Car_Rotation * DEG2RAD);

         -- Forward movement
         if Raylib.IsKeyDown (Raylib.KEY_Y) then
            Car_Position.x := Car_Position.x + Car_Speed * Car_Forward.x;
            Car_Position.z := Car_Position.z + Car_Speed * Car_Forward.z;
         end if;

         -- Backward movement
         if Raylib.IsKeyDown (Raylib.KEY_H) then
            Car_Position.x := Car_Position.x - Car_Speed * Car_Forward.x;
            Car_Position.z := Car_Position.z - Car_Speed * Car_Forward.z;
         end if;

         Car_Rot_Angle := Car_Rotation;

         -- Collision detection
         declare
            Mesh_Size       : constant Storage_Offset := Raylib.Mesh'Size / 8;
            Meshes_Address  : constant System.Address := Outer_Boundary.meshes.all'Address;
            Identity_Matrix : constant Raylib.Matrix := (
               m0  => 1.0, m4 => 0.0, m8  => 0.0,  m12 => 0.0,
               m1  => 0.0, m5 => 1.0, m9  => 0.0,  m13 => 0.0,
               m2  => 0.0, m6 => 0.0, m10 => 1.0,  m14 => 0.0,
               m3  => 0.0, m7 => 0.0, m11 => 0.0,  m15 => 1.0
            );
            Pos             : constant Raylib.Vector3 := Car_Position;
            Direction       : constant Raylib.Vector3 := Car_Forward;
            Ray             : Raylib.Ray := (
               position  => Pos,
               direction => Direction
            );
            Ray_Color       : Raylib.Color := Raylib.BLUE;
            Ray_End_Point   : Raylib.Vector3 := Vector3Add (Ray.position, Vector3Scale (Ray.direction, 5.0));
         begin
            -- 光线和赛道网格的碰撞检测
            for i in 0 .. Integer(Outer_Boundary.meshCount - 1) loop
               declare
                  Offset               : constant Storage_Offset := Storage_Offset(i) * Mesh_Size;
                  Current_Mesh_Address : constant System.Address := Meshes_Address + Offset;
                  Mesh_Ptr             : constant access Raylib.Mesh := Mesh_Ptr_Conv.To_Pointer(Current_Mesh_Address);
                  Collision            : Raylib.RayCollision;
               begin
                  -- 扩大光线范围
                  Ray_End_Point := Vector3Add(Ray.position, Vector3Scale(Ray.direction, 50.0));
      
                  -- 调试输出光线和网格
                  Put_Line("Checking Ray with Mesh " & i'Image);
                  Raylib.DrawLine3D(Ray.position, Ray_End_Point, Ray_Color);
                  Raylib.DrawBoundingBox(Raylib.GetMeshBoundingBox(Mesh_Ptr.all), Raylib.GREEN);
      
                  -- 检测光线与网格的碰撞
                  Collision := Raylib.GetRayCollisionMesh(Ray, Mesh_Ptr.all, Identity_Matrix);
                  if Collision.hit then
                     Put_Line("Collision detected with Mesh " & i'Image);
                     Car_Position := Previous_Position;
                     Ray_Color := Raylib.RED;
                     exit;
                  end if;
               end;
            end loop;


            -- Draw the collision detection ray
            Raylib.DrawLine3D (Ray.position, Ray_End_Point, Ray_Color);
         end;

         -- Draw the car model
         declare
            Rot_Axis : constant Raylib.Vector3 := (0.0, 1.0, 0.0);
            Scale    : constant Raylib.Vector3 := (0.2, 0.2, 0.2);  -- Adjusted to match World_Scale
         begin
            Raylib.DrawModelEx (Car, Car_Position, Rot_Axis, Car_Rot_Angle, Scale,
                                Raylib.WHITE);
         end;

         Raylib.DrawGrid (10, 1.0);

         Raylib.EndMode3D;

         -- Display collision distance
         if Collision_Hit then
            declare
               Distance_Text : String := "Collision Distance: " & CFloat'Image (Collision_Distance);
            begin
               Raylib.DrawText (New_String (Distance_Text), 10, 40, 20, Raylib.RED);
            end;
         end if;

         Raylib.EndDrawing;
      end;
   end loop;

   Raylib.CloseWindow;
end Raylib_Gym;
