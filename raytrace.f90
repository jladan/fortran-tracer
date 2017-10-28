program ray_trace
    use pnm
    use scene

    implicit none

    integer, parameter :: height=512, width=512
    real, dimension(width,height,3) :: image
    integer, dimension(width,height,3) :: int_image
    real,parameter :: aspect_ratio = width/height
    ! View_plane properties
    real,parameter :: view_len = width / tan(fov/2)/2
    real, dimension(3) :: hor,top_pixel
    ! ray tracing variables
    real, dimension(3) :: ray, pixel, subpixel
    real :: colour(3), tmp_colour(3)
    
    integer :: i,j,ii,jj,naa=4

    ! Set up the ray tracer geometry
    ! view_len = ubound(image, 1) / tan(fov/2.0) / 2.0;

    call cross(hor, cam_direction, ver)
    call normalize( ver )
    call normalize( hor )
    
    ! Establish view-plane location
    top_pixel = cam_position + cam_direction*view_len + ver*height/2 -hor*width/2;

    ! Now, to do the tracing
    do i = 1,ubound(image,2)
        do j = 1,ubound(image,1)
            colour = 0
            pixel = top_pixel + hor*aspect_ratio*j - ver*i
            do ii = 1,naa
                do jj = 1,naa
                    subpixel = pixel + hor*aspect_ratio*jj/naa - ver*ii/naa
                    ray = subpixel - cam_position
                    call trace(tmp_colour, ray, cam_position)
                    colour = colour + tmp_colour
                end do
            end do
            image(j,i,:) = colour/naa**2
        end do
    end do

    int_image = nint(image*255)
    int_image = min(int_image, 255)
    call write_ppm('./traced.ppm', int_image)

contains
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

    subroutine trace(colour, ray, origin)
        real, dimension(3), intent(in) :: ray,origin
        real, intent(out) :: colour(3)
        real, dimension(3) :: normal, intersection, shadow_ray, tmp, diffuse
        real :: ray_norm(3), specular(3)
        real :: raylen, diffuse_coeff, spec_coeff
        integer :: i, sphere, tmp_int

        call intersect_spheres(ray, origin, raylen, normal, sphere)

        if (raylen == 0) then
            colour = 0
        else
            diffuse = 0
            specular = 0
            intersection = ray*raylen + origin
            call normalize(normal)
            ray_norm = ray
            call normalize(ray_norm)
            

            do i = 1,ubound(lights, 2)
                shadow_ray = lights(1:3,i) - intersection
                call intersect_spheres(shadow_ray, intersection, raylen, tmp, tmp_int)
                if (raylen == 0) then
                    call normalize(shadow_ray)
                    diffuse_coeff = dot_product(normal, shadow_ray)
                    if (diffuse_coeff >0) then
                        tmp = mats(1:3,sphere_mats(sphere))*diffuse_coeff
                        tmp = tmp*lights(4:6,i)
                        diffuse = diffuse + tmp
                    end if
                    shadow_ray = shadow_ray - normal*2*diffuse_coeff
                    raylen = dot_product(shadow_ray, ray_norm)
                    spec_coeff = raylen**mats(7,sphere_mats(sphere))
                    if (spec_coeff>0) then
                        tmp = mats(4:6,sphere_mats(sphere))*spec_coeff
                        tmp = tmp * lights(4:6,i)
                        specular = specular + tmp
                    end if
                end if
            end do
            colour = ambient* mats(1:3,sphere_mats(sphere)) + diffuse+specular
        end if
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

end program ray_trace
