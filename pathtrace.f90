program path_trace
    use pnm
    use scene_path

    implicit none

    real, parameter, dimension(3,3) :: identity = reshape( &
                (/ 1,0,0,0,1,0,0,0,1 /), shape(identity) )

    integer, parameter :: height=512, width=512
    real, dimension(width,height,3) :: image
    integer, dimension(width,height,3) :: int_image
    real, parameter :: aspect_ratio = width/height
    ! View_plane properties
    real, parameter :: view_len = width / tan(fov/2)/2
    real, dimension(3) :: hor,top_pixel
    ! ray tracing variables
    real, dimension(3) :: ray, pixel
    real :: colour(3)
    
    ! Set up the ray tracer geometry
    call cross(hor, cam_direction, ver)
    call normalize( ver )
    call normalize( hor )
    
    ! Establish view-plane location
    top_pixel = cam_position + cam_direction*view_len + ver*height/2 -hor*width/2;

    ! Seed random variable
    rand(0)
    ! Now, to do the tracing
    call trace_loop(img, hor, ver, top_pixel)

    int_image = nint(image*255)
    int_image = min(int_image, 255)
    call write_ppm('/scratch/jladan/traced.ppm', int_image)

contains
    subroutine trace_loop(img, hor, ver, top_pixel)
        real, intent(out) :: img(:,:)
        real, intent(in) :: hor(3), ver(3), top_pixel(3)

        integer :: i, j

        do i = 1,ubound(image,2)
            do j = 1,ubound(image,1)
                colour = 0
                pixel = top_pixel + hor*aspect_ratio*j - ver*i
                ray = pixel - cam_position
                call trace(colour, ray, cam_position)
            end do
        end do
    end subroutine

    subroutine trace(colour, ray, origin)
        real, intent(in) :: ray(3), origin(3)
        real, intent(out) :: colour(3)

        call trace_workhorse(colour, ray, origin, max_bounces)
    end subroutine

    subroutine trace_workhorse(colour, ray, origin)
        real, intent(in) :: ray(3), origin(3)
        real, intent(out) :: colour(3)
        real, dimension(3) :: normal, intersection, new_ray, tmp_colour
        real, dimension(3) :: material_colour
        real :: raylen
        integer :: i, sphere
        logical flag

        call intersect_nodes(ray, origin, raylen, normal, node, flag)

        colour=0
        if (flag) then              ! intersection
            if (node_type(node) ==  light_type) then
                colour = node_colour(node)
            else
                intersection = ray*raylen + origin
                do i = 1,max_casts
                    call scatter_ray(normal, new_ray)
                    call trace_workhorse(tmp_colour, new_ray, intersection)
                    colour = colour + tmp_colour * node_colour(node)
                end do
                colour = colour/max_casts(3)
            end if
        end if
    end subroutine

    subroutine scatter_ray(ray, normal, new_ray)
        real, intent(in) :: normal(3), ray(3)
        real, intent(out) :: new_ray(3)

        real :: binormal(3), phi, theta
        real :: tilt_matrix, rot_matrix

        ! determine our other axis for rotation
        call cross(binormal, ray, normal) 
        ! just reflect anywhere in a hemisphere for now.
        phi = (rand(0)-0.5) * pi
        theta = rand(0) * 2*pi
        tilt_matrix = rotation_matrix(binormal,phi)
        rot_matrix = rotation_matrix(normal, theta)

        new_ray = matmul(rot_matrix, matmul(tilt_matrix))

    end subroutine


    subroutine intersect_spheres(ray, origin, raylen, normal, sphere)
        real, intent(in) :: ray(3), origin(3)
        real, intent(out) :: raylen, normal(3)
        integer, intent(out) :: sphere
        integer :: k
        real :: m = 10000,tmp, tmp_normal(3)

        raylen = 0

        do k = 1,ubound(spheres,2)
            call intersect_sphere(ray, origin, spheres(:,k), tmp, tmp_normal)
            if (tmp > 0.0000001) then
                if (tmp < raylen .or. raylen==0) then
                    raylen = tmp
                    sphere = k
                    normal = tmp_normal
                end if
            end if
        end do
    end subroutine

    subroutine intersect_sphere(ray, origin, sphere, raylen, normal)
        real, intent(in) :: ray(3), origin(3), sphere(4)
        real, intent(out) :: normal(3), raylen

        real :: line(3), a, b, c, roots(2)
        integer :: numroots
        line = sphere(1:3) - origin;
        a = dot_product(ray,ray)
        b = dot_product(ray,line)
        c = dot_product(line, line)

        call quadroots(a,-2*b, c - sphere(4)**2, numroots, roots)
        select case (numroots)
            case (0)
                raylen = 0
                normal = 0
            case (1)
                call ray_epsilon_check(roots(1), ray, line, raylen, normal)
            case (2)
                if (roots(1)<roots(2)) then
                    call ray_epsilon_check(roots(1), ray, line, raylen, normal)
                else
                    call ray_epsilon_check(roots(2), ray, line, raylen, normal)
                end if
        end select
    end subroutine

    subroutine quadroots(a, b, c, numroots, roots)
        real, intent(in) :: a,b,c
        real, intent(out) :: roots(2)
        integer, intent(out) :: numroots

        real :: t

        if (a==0) then
            roots(1) = -c/b
            roots(2) = 0
            numroots = 1
        else
            t = b**2 - 4*a*c
            if (t>0) then
                roots(1) = (-b+sqrt(t)) / (2*a)
                roots(2) = (-b-sqrt(t)) / (2*a)
                numroots = 2
            else
                roots = 0
                numroots = 0
            end if
        end if
    end subroutine

    subroutine ray_epsilon_check(root, ray, line, raylen, normal)
        real, intent(in) :: root, ray(3), line(3)
        real, intent(out) :: raylen, normal(3)

        if (root > 0.0000001) then
            raylen = root
            normal = ray*root-line
        else
            raylen=0
            normal=0
        end if
    end subroutine 

    subroutine cross(tmp, x,y)
        real, dimension(3), intent(in) :: x,y
        real, dimension(3), intent(out) :: tmp

        tmp(1) = x(2)*y(3)-x(3)*y(2)
        tmp(2) = x(3)*y(1)-x(1)*y(3)
        tmp(3) = x(1)*y(2)-x(2)*y(1)
    end subroutine

    subroutine normalize(x)
        real, intent(inout) :: x(:)

        x = x / sqrt(dot_product(x,x))
    end subroutine

    function rotation_matrix(axis, theta)
        real, intent(in) :: axis(3), theta
        real :: norm(3)

        norm = axis
        call normalize(axis)
        rotation_matrix = identity*cos(theta) + sin(theta)*cross_matrix(nnorm) + (1-cos(theta)*tensor_product(nnorm)
    end function

    function cross_matrix(u)
        real, intent(in) :: u(3)
        real, intent(out) :: cross_matrix(3,3)
        real :: m(3,3)=0

        m(2,1) = u(3)
        m(1,2) = -u(3)
        m(3,1) = -u(2)
        m(1,3) = u(3)
        m(3,2) = u(1)
        m(2,3) = -u(1)

        cross_matrix = m
    end function

    function tensor_matrix(u)
        real, intent(in) :: u(3)
        real, intent(out) :: tensor_matrix(3,3)
        real :: m(3,3)
        integer :: i, j

        do i = 1,3
            do j = 1,3
                m(i,j) = u(i)*u(j)
            end do
        end do

        tensor_matrix = m
    end function

end program ray_trace
