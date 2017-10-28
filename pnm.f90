module pnm
    implicit none

contains

    subroutine read_pnm(fileName, img)
        implicit none
        character(len=*), intent(in)            :: fileName
        integer, allocatable, intent(out)   :: img(:,:,:)

        character(2)    :: magicNumber
        integer         :: fileHandle = 9

        open(unit=fileHandle,file=fileName,action='read',status='old',access='stream',form='formatted')

        read(9, '(A2)',advance='no') magicNumber
        select case (magicNumber)
            case ('P6')
                call read_ppm(9,fileName, img)
            case default
                print *, 'Unsupported format'
        end select

        close(fileHandle)
    end subroutine read_pnm


    subroutine read_ppm(u, fileName, img)
        implicit none
        integer, intent(in)                 :: u
        character(len=*), intent(in)        :: fileName
        integer, allocatable, intent(out)   :: img(:,:,:)
        character                           :: buffer(3)

        integer :: width, height, depth
        integer :: i,j,k

        print *, 'Loading a ppm image'

        if (allocated(img)) deallocate(img)

        read(u,*) width, height, depth
        print *,  width, height, depth

        if (depth /= 255) return

        allocate(img(width,height,3))
        
        inquire(u, pos=i)
        close(u)
        open(u, file=fileName, status='old', access='stream', form='unformatted')

        ! set file position at start of data
        read(u,pos=i-1) buffer(1)


        do i = 1,width
            do j = 1,height
                read(u) buffer(:)
                img(j,i,:) = iachar(buffer)
            end do
        end do

    end subroutine read_ppm

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   Subroutines for writing pnm's
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

    subroutine write_ppm(fileName, img)
        implicit none
        character(len=*), intent(in)    :: fileName
        integer, intent(in)             :: img(:,:,:)

        integer :: fileHandle = 9
        integer :: i,j,k
        character(len=4) :: h,w


        open(unit=fileHandle,file=fileName,action='write',status='replace',access='stream')

        write(h, '(I4)') ubound(img,2)
        write(w, '(I4)') ubound(img,1)
        write(fileHandle) 'P6', achar(10)
        write(fileHandle) trim(adjustl(w)), ' ', trim(adjustl(h)), achar(10)
        write(fileHandle) '225', achar(10)
        
        ! Now the actual image
        write(fileHandle) ( ( (achar(img(i,j,k)), k=1,3),i=1,ubound(img,1)), j=1,ubound(img,2) ) 
        close(fileHandle)
    end subroutine write_ppm

    subroutine write_pgm(fileName, img)
        implicit none
        character(len=*), intent(in)    :: fileName
        integer, intent(in)             :: img(:,:)

        integer :: fileHandle = 9
        integer :: i,j
        character(len=4) :: h,w


        open(unit=fileHandle,file=fileName,action='write',status='replace',access='stream')

        write(h, '(I4)') ubound(img,2)
        write(w, '(I4)') ubound(img,1)
        write(fileHandle) 'P5', achar(10)
        write(fileHandle) trim(adjustl(w)), ' ', trim(adjustl(h)), achar(10)
        write(fileHandle) '225', achar(10)
        
        ! Now the actual image
        write(fileHandle) ( ( achar(img(i,j)), i=1,ubound(img,1)), j=1,ubound(img,2) ) 
        close(fileHandle)
    end subroutine write_pgm

end module pnm
