module scene
    implicit none

    real, parameter :: pi = 3.14159265
    ! camera geometry
    real, dimension(3) :: cam_position = (/ 0.0,0.0,800.0 /)
    real, dimension(3) :: cam_direction = (/ 0.0,0.0, -1.0 /)
    real, dimension(3) :: ver = (/ 0.0, 1.0, 0.0 /)
    real, parameter :: fov = 50.0 /180*pi

    ! ambient lighting
    real, dimension(3) :: ambient = (/ 0.3, 0.3, 0.3 /)

    ! Spheres
    real, dimension(4,5) :: spheres= reshape( &
        (/ 0.0, 0.0, -400.0, 100.0, &
           200.0, 50.0, -100.0, 150.0, &
           0.0, -1200.0, -500.0, 1000.0, &
           -100.0, 25.0, -300.0, 50.0, &
           0.0, 100.0, -250.0, 25.0 /) ,shape(spheres))

    ! Materials
    real, dimension(8,4) :: mats = reshape( &
        (/ .7, 1.0, .7, .5, .7, .5, 25.0, .3, &
           .5, .5, .5, .5, .7, .5, 25.0, .3, &
           1.0, .6, .1, .5, .7, .5, 25.0, .3, &
           .7, .6, 1.0, .5, .4, .8, 25.0, .3 /), shape(mats))
    integer :: sphere_mats(5) = (/ 1,1,2,3,1 /)

    ! Lights
    real, dimension(6,2) :: lights = reshape( &
        (/ -100.0, 150.0, 400.0, 0.7, .7, .7, &
           400.0, 100.0, 150.0, .7, 0.0, .7 /), shape(lights))



end module scene
    



