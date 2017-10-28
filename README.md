# fortran-tracer
A raytracer written in Fortran

Apparently written before I learned to document my code at all. This was written during a CSC code party after a raytracing tutorial by ssalbiz.

## Capabilities

- point light-sources
- spheres
- shadows
- diffuse and specular shading
- anti-aliasing

## Compilation

Simple using gfortran,

  gfortran raytrace.f90 scene.f90 pnm.f90
  
Outputs to `traced.ppm`, because the `.ppm` format is really simple to write.
