
Logical Function s_eqi( s1, s2 )

!*****************************************************************************80
!
!! S_EQI is a case insensitive comparison of two strings for equality.
!
!  Example:
!
!    S_EQI ( 'Anjana', 'ANJANA' ) is TRUE.
!
!  Parameters:
!
!    Input, character ( len = * ) S1, S2, the strings to compare.
!
!    Output, logical S_EQI, the result of the comparison.
!
  implicit none

  character :: c1
  character :: c2
  integer ( kind = 4 ) i
  integer ( kind = 4 ) len1
  integer ( kind = 4 ) len2
  integer ( kind = 4 ) lenc
  !logical s_eqi
  character ( len = * ),intent(in):: s1
  character ( len = * ),intent(in):: s2

  len1 = len ( s1 )
  len2 = len ( s2 )
  lenc = min ( len1, len2 )

  s_eqi = .false.

  do i = 1, lenc

    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap ( c1 )
    call ch_cap ( c2 )

    if ( c1 /= c2 ) then
      return
    end if

  end do

  do i = lenc + 1, len1
    if ( s1(i:i) /= ' ' ) then
      return
    end if
  end do

  do i = lenc + 1, len2
    if ( s2(i:i) /= ' ' ) then
      return
    end if
  end do

  s_eqi = .true.

  return
end


subroutine ch_cap ( c )

!*****************************************************************************80
!
!! CH_CAP capitalizes a single character.
!
!  Parameters:
!
!    Input/output, character C, the character to capitalize.
!
  implicit none

  character c
  integer ( kind = 4 ) itemp

  itemp = ichar ( c )

  if ( 97 <= itemp .and. itemp <= 122 ) then
    c = char ( itemp - 32 )
  end if

  return
end


subroutine s_cat ( s1, s2, s3 )

!*****************************************************************************80
!
!! S_CAT concatenates two strings to make a third string.
!
!  Parameters:
!
!    Input, character ( len = * ) S1, the "prefix" string.
!
!    Input, character ( len = * ) S2, the "postfix" string.
!
!    Output, character ( len = * ) S3, the string made by
!    concatenating S1 and S2, ignoring any trailing blanks.
!
  !implicit none

  character ( len = * ) s1
  character ( len = * ) s2
  character ( len = * ) s3

  if ( s1 == ' ' .and. s2 == ' ' ) then
    s3 = ' '
  else if ( s1 == ' ' ) then
    s3 = s2
  else if ( s2 == ' ' ) then
    s3 = s1
  else
    s3 = trim ( s1 ) // trim ( s2 )
  end if

  return
end subroutine s_cat


subroutine word_next_read ( line, word, done )

!*****************************************************************************80
!
!! WORD_NEXT_READ "reads" words from a string, one at a time.
!
!  Discussion:
!
!    The following characters are considered to be a single word,
!    whether surrounded by spaces or not:
!
!      " ( ) { } [ ]
!
!  Parameters:
!
!    Input, character ( len = * ) LINE, a string, presumably containing words
!    separated by spaces.
!
!    Output, character ( len = * ) WORD.
!    If DONE is FALSE, then WORD contains the "next" word read from LINE.
!    If DONE is TRUE, then WORD is blank, because there was no more to read.
!
!    Input/output, logical DONE.
!    On input with a fresh value of LINE, set DONE to TRUE.
!    On output, the routine sets DONE:
!      FALSE if another word was read from LINE,
!      TRUE if no more words could be read (LINE is exhausted).
!
  implicit none

  logical done
  integer ( kind = 4 ) ilo
  integer ( kind = 4 ), save :: lenc = 0
  character ( len = * ) line
  integer ( kind = 4 ), save :: next = 1
  character TAB
  character ( len = * ) word

  TAB = char ( 9 )
!
!  An input value of DONE = TRUE signals a new line of text to examine.
!
  if ( done ) then

    next = 1
    done = .false.
    lenc = len_trim ( line )

    if ( lenc <= 0 ) then
      done = .true.
      word = ' '
      return
    end if

  end if
!
!  Beginning at index NEXT, search LINE for the next nonblank,
!  which signals the beginning of a word.
!
  ilo = next

  do
!
!  ...LINE(NEXT:) is blank.  Return with WORD = ' ' and DONE = TRUE.
!
    if ( lenc < ilo ) then
      word = ' '
      done = .true.
      next = lenc + 1
      return
    end if
!
!  If the current character is blank, skip to the next one.
!
    if ( line(ilo:ilo) /= ' ' .and. line(ilo:ilo) /= TAB ) then
      exit
    end if

    ilo = ilo + 1

  end do
!
!  ILO is the index of the next nonblank character in the string.
!
!  If this initial nonblank is a special character,
!  then that's the whole word as far as we're concerned,
!  so return immediately.
!
  if ( line(ilo:ilo) == '"' .or. line(ilo:ilo) == '(' .or. &
       line(ilo:ilo) == ')' .or. line(ilo:ilo) == '{' .or. &
       line(ilo:ilo) == '}' .or. line(ilo:ilo) == '[' .or. &
       line(ilo:ilo) == ']' ) then

    word = line(ilo:ilo)
    next = ilo + 1
    return

  end if
!
!  Now search for the last contiguous character that is not a
!  blank, TAB, or special character.
!
  next = ilo + 1

  do

    if ( lenc < next ) then
      word = line(ilo:next-1)
      return
    end if

    if ( line(next:next) == ' ' .or. &
         line(next:next) == TAB .or. &
         line(next:next) == '"' .or. &
         line(next:next) == '(' .or. &
         line(next:next) == ')' .or. &
         line(next:next) == '{' .or. &
         line(next:next) == '}' .or. &
         line(next:next) == '[' .or. &
         line(next:next) == ']' ) then
      exit
    end if

    next = next + 1

  end do

  word = line(ilo:next-1)

  return
end subroutine word_next_read


module Stlreader
 contains
 
subroutine stla_read ( input_file_name,  face_num, node_xyz, face_normal, ierror )

!*****************************************************************************80
!
!! STLA_READ reads graphics information from an ASCII StereoLithography file.
!
!  Parameters:
!
!    Input, character ( len = * ) INPUT_FILE_NAME, the name of the input file.
!
!    Input, integer ( kind = 4 ) FACE_NUM, the number of faces defined.
!
!    Output, real ( kind = 8 ) NODE_XYZ(3,3,FACE_NUM), the coordinates of points.
!          |‾x1‾|‾y1‾|‾z1‾|
!          |‾‾‾‾|‾‾‾‾|‾‾‾‾|
!          | x2 | y2 | z3 |
!          |‾‾‾‾|‾‾‾‾|‾‾‾‾|
!          |_x3_|_y3_|_z3_|
!
!    Output, real ( kind = 8 ) FACE_NORMAL(3,FACE_NUM), the normal vector
!    at each face.
!
!    Output, integer ( kind = 4 ) IERROR, is nonzero if an error occurred.
!
  implicit none

  integer( kind = 4 ),intent(in) ::face_num
  !integer ( kind = 4 ) node_num

  logical ::read_text, done
  real ( kind = 8 ) dval
  integer ( kind = 4 ) face
  !integer, allocatable ::  face_node(:,:)
  real , intent(inout) :: face_normal(:,:)
  integer ( kind = 4 ) i,j,ii,jj
  integer ( kind = 4 ),intent(inout):: ierror
  character ( len = 127 ), intent(in):: input_file_name
  integer ( kind = 4 ) ios
  
  integer :: lchar,node,state
  real,intent(inout) :: node_xyz(:,:,:)
  logical :: s_eqi
  real ( kind = 8 ):: temp(3,3)
  character ( len = 255 ) text
  integer ( kind = 4 ) text_num
  integer ( kind = 4 ) vertex
  
  character ( len = 127 ) word1
  character ( len = 127 ) word2


  ierror = 0
  state = 0
  text_num = 0
  face = 0
  node = 0
  
  
!
!  Open the file.
!

!input_file_name = 'Ra6.4e8_LY=0.5144_floor_ascii.stl'


! write(*,*) "Open a stl file"
! open(unit=7, file = input_file_name, status = 'old', iostat = ios)
! write(*,*) "Start counting"

! face_num = 0
! read_text = .false.

! do
	! read(7, '(a)', iostat = ios) line
	
	! if (read_text) then
		! face_num = face_num + 1
		! read_text = .false.
	! end if
	
	! read_text = (line(1:5) == 'facet')
	
	! if (line(1:8)=='endsolid') then
		! exit
	! end if
! end do
! close(unit = 7)
! write(*,*) "end counting"

! write(*,*) "The face number is :", face_num

  !allocate(face_node(3,face_num))
  !allocate(face_normal(3,face_num))
  !allocate(node_xyz(3,3,face_num))
  
  !face_node = 0
  face_normal = 0.0
  node_xyz = 0.0

  open ( unit = 8, file = input_file_name, status = 'old', iostat = ios )

  if ( ios /= 0 ) then
    write ( *, '(a)' ) ' '
    write ( *, '(a)' ) 'STLA_READ - Fatal error!'
    write ( *, '(a)' ) '  Could not open the file "' // &
      trim ( input_file_name ) // '".'
    ierror = 1
    return
  end if
!
!  Read the next line of text.
!
  do

    read ( 8, '(a)', iostat = ios ) text

    if ( ios /= 0 ) then
      if ( state /= 0 .and. state /= 1 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STLA_READ - Warning.'
        write ( *, '(a)' ) '  End-of-file, but model not finished.'
        write ( *, '(a,i8)' ) '  File line number = ', text_num
        ierror = 1
        return
      end if
      exit
    end if

    text_num = text_num + 1
    done = .true.
!
!  Read the first word in the line (text).
!
    call word_next_read ( text, word1, done )
!
!  "Doctor" the text, changing a beginning occurrence of:
!
!      END FACET to ENDFACET
!      END LOOP to ENDLOOP
!      END SOLID to ENDSOLID
!      FACET NORMAL to FACETNORMAL
!      OUTER LOOP to OUTERLOOP
!
	
	
    if ( s_eqi( word1, 'END' ) .or. &
         s_eqi( word1, 'FACET' ) .or. &
         s_eqi( word1, 'OUTER' ) ) then

      call word_next_read ( text, word2, done )
      call s_cat ( word1, word2, word1 )

    end if
!
!  This first word tells us what to do.
!
!  SOLID - begin a new solid.
!    Valid in state 0, moves to state 1.
!  ENDSOLID - end current solid.
!    Valid in state 1, moves to state 0.
!
!  FACETNORMAL - begin a new facet.
!    Valid in state 0 or 1, moves to state 2.
!  ENDFACET - end current facet.
!    Valid in state 2, moves to state 1.
!
!  OUTERLOOP - begin a list of vertices.
!    Valid in state 2, moves to state 3, sets vertex count to 0.
!  ENDLOOP - end vertex list.
!    Valid in state 3, moves to state 2.
!
!  VERTEX - give coordinates of next vertex.
!    Valid in state 3.
!
!  End of file -
!    Valid in state 0 or 1.
!	
	
    if ( s_eqi( word1, 'SOLID' ) ) then

      if ( state /= 0 ) then
        write ( *, '(a)' ) ' '
        write ( *, '(a)' ) 'STLA_READ - Warning!'
        write ( *, '(a)' ) '  Model not in right state for SOLID.'
        write ( *, '(a,i8)' ) '  File line number = ', text_num
        ierror = 1
        return
      end if

      state = 1

    else if ( s_eqi( word1, 'ENDSOLID' ) ) then

		if ( state /= 1 ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  Model not in right state for ENDSOLID.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		state = 0

    else if ( s_eqi ( word1, 'FACETNORMAL' ) ) then

			if ( state /= 0 .and. state /= 1 ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  Model not in right state for FACET.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		state = 2
		face = face + 1

		if ( face_num < face ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  More faces being read than expected.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		do i = 1, 3

        call word_next_read ( text, word2, done )
			if ( .not. done ) then
			read(word2, *) dval !
			!read(word2, '(f8.6)') dval
			if ( ierror == 0 ) then
            face_normal(i,face) = dval
          end if
        end if
      end do

    else if ( s_eqi( word1, 'ENDFACET' ) ) then 

		if ( state /= 2 ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  Model not in right state for ENDFACET.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		state = 1

    else if ( s_eqi( word1, 'OUTERLOOP' ) ) then
		
			j = 1
		
		if ( state /= 2 ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  Model not in right state for OUTERLOOP.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		state = 3
		vertex = 0
		
		read ( 8, '(a)', iostat = ios ) text
		
		 !write(*,*) 'text:', text
		 !write(*,*) 'done:', done
		  done = .true.
		 call word_next_read ( text, word1, done ) 
		
		 !write(*,*) 'word1:', word1
		 !write(*,*) 'word2:', word2
		 !write(*,*) 'j1:', j
		 
		 
		 do while ( s_eqi( word1, 'VERTEX') ) 
			!Ren read
			do i = 1, 3
				call word_next_read ( text, word2, done )
				!call s_to_r8 ( word2, dval, ierror, lchar )
				read(word2, *) dval
				temp(i,j) = dval
				!write (*,*) 'the number:', dval
			end do
			
			read ( 8, '(a)', iostat = ios ) text
			text_num = text_num + 1
			done = .true.
			!write(*,*) 'text2:', text
			call word_next_read ( text, word1, done )
			
			!write(*,*) 'word1_inloop:', word1
			j = j+1
			
			!write(*,*) 'j2:', j
			
		 end do
		 
		 node_xyz(1:3,1:3,face) = temp(1:3,1:3)
		 !write (*,*) 'word1_after loop:', word1
		 state = 2
		 
    else if ( s_eqi( word1, 'ENDLOOP' ) ) then
		
		if ( state /= 3 ) then
			write ( *, '(a)' ) ' '
			write ( *, '(a)' ) 'STLA_READ - Warning!'
			write ( *, '(a)' ) '  Model not in right state for ENDLOOP.'
			write ( *, '(a,i8)' ) '  File line number = ', text_num
			ierror = 1
			return
		end if

		state = 2


		else

		write ( *, '(a)' ) ' '
		write ( *, '(a)' ) 'STLA_READ - Warning!'
		write ( *, '(a)' ) '  Unrecognized line in file.'
		write ( *, '(a,i8)' ) '  File line number = ', text_num
		ierror = 1
		return

		end if

	end do
!
!  Close the file.
!
  close ( unit = 8 )
	! write(*,*) "Output to the file"
	! open ( 10, file = 'node_xyz.txt', status = 'new' )
		 do jj = 1, 3
			 do ii =1,3
			   write(*,*) node_xyz(ii,jj,2)
			 end do
		 end do
	! close(10)
  !return
end subroutine stla_read

end module Stlreader

module triangle
contains

subroutine triangles(face_num, node_xyz, face_normal,centriod, delta,cmap)
!************************************************************************!
!
!the target of subroutine triangles is to calculate the centriod,
! area of trangles on the stl file and the position of the centriod
! move away a fixed distance along the normal direction.
!
! INPUT: NODE_XYZ(:,:,:)
! INPUT: FACE_NORMAL(:,:)
! OUTPUT: CENTRIOD(4,FACE_NUM): 1=X, 2=Y, 3=Z, 4=AREA
! OUTPUT: CMAP(3:FACE_NUM): 1=X, 2=Y, 3=Z 
!
! Area of a triangle: A= sqrt(s*(s-a)*(s-b)*(s-c)) (Heron's formula)
!
!
!************************************************************************!
implicit none
integer, intent(in):: face_num
real, intent(in) :: node_xyz(:,:,:)
real, intent(in) :: face_normal(:,:)
real, intent(out) :: centriod(:,:)
real, intent(out) :: cmap(:,:)
real(kind=8), intent(in) :: delta
real(kind=8):: s, a,b,c, dx,dy,dz
integer:: i,j,k,ii,jj
real(kind=8) :: temp1, temp2

!allocate (centriod(4,face_num))
!allocate (cmap(3,face_num))

centriod = 0.0
cmap = 0.0
s = 0.0
a = 0.0
b = 0.0
c = 0.0

temp1 = 1.0/3.0
temp2 = 0.0

do j = 1,face_num 
	do i=1,3
	centriod(i,j) = temp1*(node_xyz(i,1,j)+node_xyz(i,2,j)+node_xyz(i,3,j))
	end do
	a = sqrt((node_xyz(1,1,j)-node_xyz(1,2,j))**2.0+&
		(node_xyz(2,1,j)-node_xyz(2,2,j))**2.0+&
		(node_xyz(3,1,j)-node_xyz(3,2,j))**2.0)
	
	b = sqrt((node_xyz(1,2,j)-node_xyz(1,3,j))**2.0+&
		(node_xyz(2,2,j)-node_xyz(2,3,j))**2.0+&
		(node_xyz(3,2,j)-node_xyz(3,3,j))**2.0)
		
	c = sqrt((node_xyz(1,1,j)-node_xyz(1,3,j))**2.0+&
		(node_xyz(2,1,j)-node_xyz(2,3,j))**2.0+&
		(node_xyz(3,1,j)-node_xyz(3,3,j))**2.0)
	
	s = 0.5*(a+b+c)
	! Ren
	! if (j == 15779) then
		! write(*,*) 'the node x1:', node_xyz(1,1,j)
		! write(*,*) 'the node y1:', node_xyz(1,2,j)
		! write(*,*) 'the node z1:', node_xyz(1,3,j)
		
		! write(*,*) 'the node x2:', node_xyz(2,1,j)
		! write(*,*) 'the node y2:', node_xyz(2,2,j)
		! write(*,*) 'the node z2:', node_xyz(2,3,j)
		
		! write(*,*) 'the node x3:', node_xyz(3,1,j)
		! write(*,*) 'the node y3:', node_xyz(3,2,j)
		! write(*,*) 'the node z3:', node_xyz(3,3,j)
		
		! write(*,*) 'the edge length a:', a
		! write(*,*) 'the edge length b:', b
		! write(*,*) 'the edge length c:', c
		! write(*,*) 'the edge length s:', s
	! end if
	!!!!!!!!!!!!!!!!!
	centriod(4,j) = (sqrt(abs(s*(s-a)*(s-b)*(s-c))))
	
	! temp2 = delta/sqrt(face_normal(1,j)**2.0 +&
			! face_normal(2,j)**2.0 +&
			! face_normal(3,j)**2.0)
			
	do k = 1,3		
		cmap(k,j) = centriod(k,j)
	end do
	
end do
!Ren: Error Facet
	! write(*,*) 'the error facet A:', centriod(4,15779)
	! write(*,*) 'the error facet x:', centriod(1,15779)
	! write(*,*) 'the error facet y:', centriod(2,15779)
	! write(*,*) 'the error facet z:', centriod(3,15779)

write(*,*) 'output some results'
 do ii = 1,2
	write(*,*) 'centriod_face1', centriod(4,ii)
	write(*,*) 'cmap1', cmap(ii,1)
end do


end subroutine triangles
end module triangle

module T_G
contains

subroutine GradT(qcel, x,y,z, cmap, TG, Tsurf, Psurf, Shear_stress, P_force, V_force, delta,&
				 T_sur, mu0, Pr, Cv, Kcpv, R, rho0, p0, T0, T_fix, Q_fix,&
                 face_num, n_cube, n_cellx, n_celly, n_cellz, face_normal)
				 
				 


!*******************************************************************!
!
! Subroutine GradT: according to the points from Subroutine Triangles,
! calculating the temperature gradient on the STL files.
!
!
!*******************************************************************!

implicit none

real, intent(in) :: qcel(:,:,:,:,:)
real, intent(in) :: x(:,:,:,:),y(:,:,:,:),z(:,:,:,:)
real, intent(inout) :: cmap(:,:)
real :: cube_xp, cube_xm, cube_yp, cube_ym, cube_zp, cube_zm
real, intent(out), allocatable ::  TG(:), Tsurf(:), Psurf(:), Shear_stress(:)
real, intent(out), allocatable ::  P_force(:,:), V_force(:,:)
real, allocatable ::  vtemp(:,:), vntemp(:,:), tt(:,:)
!real :: T_cmap
real(kind = 8), intent(in) :: delta, T_sur, mu0, Pr, Cv, Kcpv, R, rho0, p0, T0, T_fix, Q_fix
real :: vl, vn, vt, vt_inv, mu
integer, intent(in) :: face_num
integer, intent(in) :: n_cube, n_cellx, n_celly, n_cellz
real, intent(in) :: face_normal(:,:)
integer :: h,i,j,k,l,ii,jj,kk, iii, jjj, kkk
real :: i_,j_, k_
real :: cellsize, temp, h_sum
real :: h_mmm, h_pmm, h_mpm, h_mmp, h_ppm, h_pmp, h_mpp, h_ppp !weight of 8 corners.
real :: T_mmm, T_pmm, T_mpm, T_mmp, T_ppm, T_pmp, T_mpp, T_ppp !Temperatures at 8 corners.
real :: p_mmm, p_pmm, p_mpm, p_mmp, p_ppm, p_pmp, p_mpp, p_ppp !P at 8 corners.
real :: rho, ee, u, v, w, VV, p, T
real :: u_mmm, u_pmm, u_mpm, u_mmp, u_ppm, u_pmp, u_mpp, u_ppp !u at 8 corners.
real :: v_mmm, v_pmm, v_mpm, v_mmp, v_ppm, v_pmp, v_mpp, v_ppp !v at 8 corners.
real :: w_mmm, w_pmm, w_mpm, w_mmp, w_ppm, w_pmp, w_mpp, w_ppp !w at 8 corners.
real :: TT0		   


allocate(TG(face_num)) ! local Temperature gradient
allocate(Shear_stress(face_num)) ! local wall stress (velocity term)
allocate(Psurf(face_num)) ! local wall stress (pressure term)
allocate(Tsurf(face_num))
allocate(P_force(3,face_num))
allocate(V_force(3,face_num))

allocate(tt(3,face_num)) ! tangential vector of facets
allocate(vtemp(3,face_num))
allocate(vntemp(3,face_num))

write(*,*) 'Mu is :', mu0

cube_xm = 0.0
cube_xp = 0.0
cube_ym = 0.0
cube_ym = 0.0
cube_zm = 0.0
cube_zm = 0.0
i = 0
j = 0
k = 0
ii = 0
jj = 0
kk = 0
iii = 0
jjj = 0
kkk = 0
i_ = 0.0
j_ = 0.0
k_ = 0.0

TT0 = 273.15			

	
	do h = 1, face_num
	
		do l = 1, n_cube
		
			cellsize = delta*abs(x(1,1,1,l)-x(2,1,1,l))
			temp = 1.0/cellsize
			cube_xm = x(1,1,1,l) 
			cube_xp = x(n_cellx,1,1,l) 
			cube_ym = y(1,1,1,l) 
			cube_yp = y(1,n_celly,1,l) 
			cube_zm = z(1,1,1,l) 
			cube_zp = z(1,1,n_cellz,l) 
			
			if (cube_xp > cmap(1,h) .and. cube_xm <= cmap(1,h)&
				.and. cube_yp > cmap(2,h) .and. cube_ym <= cmap(2,h)&
				.and. cube_zp > cmap(3,h) .and. cube_zm <= cmap(3,h)) then
				
				cmap(1,h) = cellsize*face_normal(1,h)+cmap(1,h)
				cmap(2,h) = cellsize*face_normal(2,h)+cmap(2,h)
				cmap(3,h) = cellsize*face_normal(3,h)+cmap(3,h)
				
			    EXIT  
				
			endif
				
		enddo
		
	enddo
	


write(*,*) 'the cube number is:', n_cube
do h = 1, face_num
	do l = 1, n_cube
		cellsize = abs(x(1,1,1,l)-x(2,1,1,l))
		temp = 1.0/cellsize
		cube_xm = x(1,1,1,l) 
		cube_xp = x(n_cellx,1,1,l) 
		cube_ym = y(1,1,1,l) 
		cube_yp = y(1,n_celly,1,l) 
		cube_zm = z(1,1,1,l) 
		cube_zp = z(1,1,n_cellz,l) 
		if (cube_xp > cmap(1,h) .and. cube_xm <= cmap(1,h)&
			.and. cube_yp > cmap(2,h) .and. cube_ym <= cmap(2,h)&
			.and. cube_zp > cmap(3,h) .and. cube_zm <= cmap(3,h)) then
				ii = floor((cmap(1,h)-cube_xm)*temp)
				jj = floor((cmap(2,h)-cube_ym)*temp)
				kk = floor((cmap(3,h)-cube_zm)*temp)
				
				i = ii+1
				j = jj+1
				k = kk+1
				
				if(i + 1 > 17 .or. j + 1 > 17 .or. k + 1 > 17)then
					write(*,*) 'Error in locating points in the Cube'
					write(*,*) 'i is:', i
					write(*,*) 'j is:', j
					write(*,*) 'k is:', k
					write(*,*) 'the face number is:', h
					write(*,*) 'the Cube number is:', l
					cycle
				end if 
				
				! Trilinear Interpolation
				h_mmm = abs(cmap(1,h)-x(i+1,j+1,k+1,l))*abs(cmap(2,h)-y(i+1,j+1,k+1,l))*abs(cmap(3,h)-z(i+1,j+1,k+1,l))
				h_pmm = abs(cmap(1,h)-x(i,j+1,k+1,l))*abs(cmap(2,h)-y(i,j+1,k+1,l))*abs(cmap(3,h)-z(i,j+1,k+1,l))
				h_mpm = abs(cmap(1,h)-x(i+1,j,k+1,l))*abs(cmap(2,h)-y(i+1,j,k+1,l))*abs(cmap(3,h)-z(i+1,j,k+1,l))
				h_mmp = abs(cmap(1,h)-x(i+1,j+1,k,l))*abs(cmap(2,h)-y(i+1,j+1,k,l))*abs(cmap(3,h)-z(i+1,j+1,k,l))
				h_ppm = abs(cmap(1,h)-x(i,j,k+1,l))*abs(cmap(2,h)-y(i,j,k+1,l))*abs(cmap(3,h)-z(i,j,k+1,l))
				h_pmp = abs(cmap(1,h)-x(i,j+1,k,l))*abs(cmap(2,h)-y(i,j+1,k,l))*abs(cmap(3,h)-z(i,j+1,k,l))
				h_mpp = abs(cmap(1,h)-x(i+1,j,k,l))*abs(cmap(2,h)-y(i+1,j,k,l))*abs(cmap(3,h)-z(i+1,j,k,l))
				h_ppp = abs(cmap(1,h)-x(i,j,k,l))*abs(cmap(2,h)-y(i,j,k,l))*abs(cmap(3,h)-z(i,j,k,l))
				
				h_sum = 1.0/(h_mmm + h_pmm + h_mpm + h_mmp + h_ppm + h_pmp + h_mpp + h_ppp)
				
				rho = qcel(1,i,j,k,l)
				u_mmm = qcel(2,i,j,k,l) / rho
				v_mmm = qcel(3,i,j,k,l) / rho
				w_mmm = qcel(4,i,j,k,l) / rho
				ee =  qcel(5, i, j, k, l)
				VV = u_mmm*u_mmm + v_mmm*v_mmm + w_mmm*w_mmm 
				p_mmm = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_mmm = p_mmm/(rho*R)
				
				rho = qcel(1,i+1,j,k,l)
				u_pmm = qcel(2,i+1,j,k,l) / rho
				v_pmm = qcel(3,i+1,j,k,l) / rho
				w_pmm = qcel(4,i+1,j,k,l) / rho
				ee =  qcel(5, i+1, j, k, l)
				VV = u_pmm*u_pmm + v_pmm*v_pmm + w_pmm*w_pmm 
				p_pmm = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_pmm = p_pmm/(rho*R)
				
				rho = qcel(1,i,j+1,k,l)
				u_mpm = qcel(2,i,j+1,k,l) / rho
				v_mpm = qcel(3,i,j+1,k,l) / rho
				w_mpm = qcel(4,i,j+1,k,l) / rho
				ee =  qcel(5, i, j+1, k, l)
				VV = u_mpm*u_mpm + v_mpm*v_mpm + w_mpm*w_mpm 
				p_mpm = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_mpm = p_mpm/(rho*R)
				
				rho = qcel(1,i,j,k+1,l)
				u_mmp = qcel(2,i,j,k+1,l) / rho
				v_mmp = qcel(3,i,j,k+1,l) / rho
				w_mmp = qcel(4,i,j,k+1,l) / rho
				ee =  qcel(5, i, j, k+1, l)
				VV = u_mmp*u_mmp + v_mmp*v_mmp + w_mmp*w_mmp 
				p_mmp = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_mmp = p_mmp/(rho*R)
				
				rho = qcel(1,i+1,j+1,k,l)
				u_ppm = qcel(2,i+1,j+1,k,l) / rho
				v_ppm = qcel(3,i+1,j+1,k,l) / rho
				w_ppm = qcel(4,i+1,j+1,k,l) / rho
				ee =  qcel(5, i+1, j+1, k, l)
				VV = u_ppm*u_ppm + v_ppm*v_ppm + w_ppm*w_ppm 
				p_ppm = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_ppm = p_ppm/(rho*R)
				
				rho = qcel(1,i+1,j,k+1,l)
				u_pmp = qcel(2,i+1,j,k+1,l) / rho
				v_pmp = qcel(3,i+1,j,k+1,l) / rho
				w_pmp = qcel(4,i+1,j,k+1,l) / rho
				ee =  qcel(5, i+1, j, k+1, l)
				VV = u_pmp*u_pmp + v_pmp*v_pmp + w_pmp*w_pmp 
				p_pmp = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_pmp = p_pmp/(rho*R)
				
				rho = qcel(1,i,j+1,k+1,l)
				u_mpp = qcel(2,i,j+1,k+1,l) / rho
				v_mpp = qcel(3,i,j+1,k+1,l) / rho
				w_mpp = qcel(4,i,j+1,k+1,l) / rho
				ee =  qcel(5, i, j+1, k+1, l)
				VV = u_mpp*u_mpp + v_mpp*v_mpp + w_mpp*w_mpp 
				p_mpp = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_mpp = p_mpp/(rho*R)
				
				rho = qcel(1,i+1,j+1,k+1,l)
				u_ppp = qcel(2,i+1,j+1,k+1,l) / rho
				v_ppp = qcel(3,i+1,j+1,k+1,l) / rho
				w_ppp = qcel(4,i+1,j+1,k+1,l) / rho
				ee =  qcel(5, i+1, j+1, k+1, l)
				VV = u_ppp*u_ppp + v_ppp*v_ppp + w_ppp*w_ppp 
				p_ppp = (ee-0.5*rho*VV)*(Kcpv - 1.0d0)
				T_ppp = p_ppp/(rho*R)
				
				T = (h_mmm*T_mmm + h_pmm*T_pmm + h_mpm*T_mpm + h_mmp*T_mmp &
					+ h_ppm*T_ppm + h_pmp*T_pmp + h_mpp*T_mpp + h_ppp*T_ppp) &
					* h_sum
						
				p = (h_mmm*p_mmm + h_pmm*p_pmm + h_mpm*p_mpm + h_mmp*p_mmp &
					+ h_ppm*p_ppm + h_pmp*p_pmp + h_mpp*p_mpp + h_ppp*p_ppp) &
					* h_sum
					
				u = (h_mmm*u_mmm + h_pmm*u_pmm + h_mpm*u_mpm + h_mmp*u_mmp &
					+ h_ppm*u_ppm + h_pmp*u_pmp + h_mpp*u_mpp + h_ppp*u_ppp) &
					* h_sum
				
				v = (h_mmm*v_mmm + h_pmm*v_pmm + h_mpm*v_mpm + h_mmp*v_mmp &
					+ h_ppm*v_ppm + h_pmp*v_pmp + h_mpp*v_mpp + h_ppp*v_ppp) &
					* h_sum
				
				w = (h_mmm*w_mmm + h_pmm*w_pmm + h_mpm*w_mpm + h_mmp*w_mmp &
					+ h_ppm*w_ppm + h_pmp*w_pmp + h_mpp*w_mpp + h_ppp*w_ppp) &
					* h_sum
									
				mu = mu0*((T/TT0)**1.5)*(TT0+110.4)/(T + 110.4)
					
				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
				! this may be WRONG
				!TG(h) = (T_sur - T)/delta * face_normal(1,h) ! oriented along width direction of the caivity
				!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!	
				
				vtemp(1,h) = u
				vtemp(2,h) = v
				vtemp(3,h) = w
				
				vl = u**2.0 + v**2.0 + w**2.0
				
				vn = (face_normal(1,h)*vtemp(1,h) + face_normal(2,h)*vtemp(2,h) + face_normal(3,h)*vtemp(3,h))&
						/sqrt(face_normal(1,h)**2.0 +face_normal(2,h)**2.0 +face_normal(3,h)**2.0)
				
				do iii = 1, 3
					vntemp(iii,h) = vn*face_normal(iii,h)
				end do
				
				u = u-vntemp(1,h)
				v = v-vntemp(2,h)
				w = w-vntemp(3,h)
				
				vt = sqrt((u**2.0 + v**2.0 + w**2.0))
				
				Shear_stress(h) = mu*vt/(delta*cellsize)
				V_force(1,h) = sign(Shear_stress(h),u)
				V_force(2,h) = sign(Shear_stress(h),v)
				V_force(3,h) = sign(Shear_stress(h),w)
				
				Psurf(h) = p 
				P_force(1,h) = -p * face_normal(1,h)
				P_force(2,h) = -p * face_normal(2,h)
				P_force(3,h) = -p * face_normal(3,h)
				
				Tsurf(h) = T+Q_fix/(mu*Cv*Kcpv/Pr)*(delta*cellsize)  ! for Iso heat flux
				
				TG(h) = (mu*Cv*Kcpv/Pr)*(T_fix - T)/(delta*cellsize)  ! for Isothemral  
				
				if (h .eq. 100) then
					write(*,*) 'at the facet ', h
					write(*,*) 'T is:', T
					write(*,*) 'P is:', p
					write(*,*) 'u is:', u
					write(*,*) 'v is:', v
					write(*,*) 'w is:', w
					write(*,*) '1/h_sum is:',h_sum
					write(*,*) 'tt_x is:', tt(1,h)
					write(*,*) 'tt_y is:', tt(2,h)
					write(*,*) 'tt_z is:', tt(3,h)
					write(*,*) 'Vt is:', vt
				end if
				
				exit
		end if
		
		! if (l == 2) then
			! write(*,*) 'the TG at first cube:', TG(h)
			! write(*,*) 'the facet number is :', h
			! write(*,*) 'the cell size is:', cellsize
			! write(*,*) 'the cube xm is:', cube_xm
			! write(*,*) 'the cube xp is:', cube_xp
			! write(*,*) 'the cube ym is:', cube_ym
			! write(*,*) 'the cube yp is:', cube_yp
			! write(*,*) 'the cube zm is:', cube_zm
			! write(*,*) 'the cube zp is:', cube_zp
			! write(*,*) 'the i:', i
			! write(*,*) 'the j:', j
			! write(*,*) 'the k:', k
		! end if
	end do

end do


end subroutine GradT
end module T_G

program stl_detail
!*************************************************************!
!
!   unit 7 and unit 8: open and close the stl file
!   unit 9: open and close q file
!   unit 1: output data
!
!*************************************************************!
use Stlreader
use triangle
use T_G

implicit none

character ( len = 255 ) :: input_stl_name
character ( len = 255 ) :: input_mesh_name
character ( len = 255 ) :: input_field_name
integer :: face_num
logical :: read_text
character(len=127) :: line
logical :: eqi_str
real, allocatable :: node_xyz(:,:,:)
real, allocatable :: face_normal(:,:)
real, allocatable :: centriod(:,:)
real, allocatable :: cmap(:,:), TG(:), ss(:), ssp(:)
real, allocatable :: Tsurf(:),Psurf(:),Shear_stress(:)
real, allocatable :: P_force(:,:),V_force(:,:)

real(kind=8) :: delta, T_sur, base_x
integer :: ios, ierror

integer :: n_cube, n_cellx, n_celly, n_cellz
integer :: i,j,k,l,q,icube,ii,jj,kk,hh,ww,xx,yy,zz,icount
real :: rdum, height, h_temp
real, allocatable :: qcel(:,:,:,:,:)
real, allocatable :: x(:,:,:,:)  ! mesh
real, allocatable :: y(:,:,:,:)  ! mesh
real, allocatable :: z(:,:,:,:)  ! mesh

integer :: p3dq(5) = (/1,2,3,4,5/)

real :: TG_avg, TS_avg, Area, Pfx_avg, Pfy_avg, Pfz_avg, Vfx_avg, Vfy_avg, Vfz_avg
real :: TG_up_avg

real(kind = 8) :: mu0, Pr, Cv, Kcpv, R, rho0, p0, T0, T_fix, Q_fix

real, allocatable:: Tg_local(:), Area_local(:), ss_local(:), ssp_local(:)

allocate(Tg_local(16))
allocate(Area_local(16))
allocate(ss_local(16))
allocate(ssp_local(16))

!!!! ================ USER INPUT ================ !!!!

  input_stl_name = 'sphere_0.01m.stl'
  input_mesh_name = 'mesh_less.g'
  input_field_name = 'field_0000000418_RE100.q'

  delta = sqrt(3d0)  ! sqrt(2)*dx is the best
  
  T_fix = 0.0d0
  Q_fix = 20000d0

  mu0 = 0.00185
  Pr = 0.72
  Cv = 717.5
  Kcpv = 1.4d0
  R = 287d0
  
  rho0 = 1.1842d0
  p0 = 101300d0
  T0 = p0/R/rho0

  T_sur = T0
  height = 0.5144
  base_x = -10e-15  ! detect peak or valley
  
!!!! ================ USER INPUT ================ !!!!
  
  
  TG_avg = 0.0
  TS_avg = 0.0
  Area = 0.0
  Tg_local = 0.0
  Area_local = 0.0
  
  Pfx_avg = 0.0d0
  Pfy_avg = 0.0d0 
  Pfz_avg = 0.0d0
  
  Vfx_avg = 0.0d0
  Vfy_avg = 0.0d0
  Vfz_avg = 0.0d0
  
  
! open STL file!
write(*,*) "Open a stl file"
open(unit=7, file = input_stl_name, status = 'old', iostat = ios)
write(*,*) "Start counting face number"

face_num = 0
ierror = 0
read_text = .false.

do
	read(7, '(a)', iostat = ios) line
	
	if (read_text) then
		face_num = face_num + 1
		read_text = .false.
	end if
	
	read_text = (line(1:5) == 'facet')
	
	if (line(1:8)=='endsolid') then
		exit
	end if
end do
	close(unit = 7)
write(*,*) "end counting"

write(*,*) "The face number is :", face_num

  allocate(face_normal(3,face_num))
  allocate(node_xyz(3,3,face_num))
  allocate(centriod(4,face_num))
  allocate(cmap(3,face_num))
  
  allocate(Tsurf(face_num))
  allocate(Psurf(face_num))
  allocate(TG(face_num))
  allocate(Shear_stress(face_num))
  
  allocate(P_force(3,face_num))
  allocate(V_force(3,face_num))


call stla_read(input_stl_name, face_num, node_xyz, face_normal, ierror )
call triangles(face_num, node_xyz, face_normal,centriod, delta,cmap)

! open and read flow field file
write(*,*) "Start reading mesh data..."
	open(unit=20, form='unformatted', file=input_mesh_name ,convert='little_endian')	   
	read(20) n_cube
	read(20) (n_cellx, n_celly, n_cellz, i = 1, n_cube) ! n_cellx = n_celly = n_cellz = 17
	allocate(x(n_cellx, n_celly, n_cellz, n_cube))
	allocate(y(n_cellx, n_celly, n_cellz, n_cube))
	allocate(z(n_cellx, n_celly, n_cellz, n_cube))
	
	write(*,*) 'the cube number is:', n_cube
	
	x=0.0
	y=0.0
	z=0.0		
	do i = 1, n_cube
		read(20) &		
           ((( x(j,k,l,i), j=1,n_cellx), k=1,n_celly), l=1,n_cellz),&
           ((( y(j,k,l,i), j=1,n_cellx), k=1,n_celly), l=1,n_cellz),&
           ((( z(j,k,l,i), j=1,n_cellx), k=1,n_celly), l=1,n_cellz)
	end do	
		
	close(20)
	write(*,*) "End reading mesh data"
!---------------------------------------------------------  
	write(*,*) "Start reading field data ..."
 
  open ( unit=20, form='unformatted', file=input_field_name, convert='little_endian')
  read(20) n_cube
  read(20) (n_cellx, n_celly, n_cellz, i = 1, n_cube)
  
  allocate(qcel(5, n_cellx, n_celly, n_cellz, n_cube))
  
  qcel = 0.0
  
  write(*,*) 'the cell number in a cube:', n_cellx
  
  do i = 1, n_cube
    read(20) rdum, rdum, rdum, rdum
    read(20) &
        ((((qcel(p3dq(q), j, k, l, i), &
        j = 1, n_cellx), k = 1, n_celly), l = 1, n_cellz), &
        q = 1, 5)
  end do	  
  close(20)
  
	write(*,*) "End reading field data"
	
	call GradT( qcel, x,y,z, cmap, TG, Tsurf, Psurf, Shear_stress, P_force, V_force, delta, &
	            T_sur, mu0, Pr, Cv, Kcpv, R, rho0, p0, T0, T_fix, Q_fix, &
	            face_num, n_cube, n_cellx, n_celly, n_cellz, face_normal)
				
				
	write(*,*) 'output local average TG...'
	
	! do hh = 1, 16
		 ! do ii = 1, face_num	
			! if (((hh-1)*h_temp - height/2.0) <= centriod(2,ii) .and. &
				! (hh*h_temp- height/2.0) > centriod(2,ii)) then
				! Tg_local(hh) = Tg_local(hh) + TG(ii)*centriod(4,ii)
				! ss_local(hh) = ss_local(hh) + ss(ii)*centriod(4,ii)
				! Area_local(hh) = Area_local(hh) + centriod(4,ii)
			! endif
		! end do
		! !write(*,*) 'facet NUMBER:', ii
		! !write(*,*) 'facet AREA:', centriod(4,ii)
	! end do
	
	
	
	do ii = 1, face_num
	
		TG_avg = TG_avg + TG(ii)*centriod(4,ii)
		TS_avg = TS_avg + Tsurf(ii)*centriod(4,ii)
		
		! ss_avg = ss_avg + ss(ii)*centriod(4,ii)
		! ssp_avg = ssp_avg + ssp(ii)*centriod(4,ii)
		
		Pfx_avg = Pfx_avg + P_force(1,ii)*centriod(4,ii)
		Pfy_avg = Pfy_avg + P_force(2,ii)*centriod(4,ii)
		Pfz_avg = Pfz_avg + P_force(3,ii)*centriod(4,ii)
		
		Vfx_avg = Vfx_avg + V_force(1,ii)*centriod(4,ii)
		Vfy_avg = Vfy_avg + V_force(2,ii)*centriod(4,ii)
		Vfz_avg = Vfz_avg + V_force(3,ii)*centriod(4,ii)
		
		Area = Area + centriod(4,ii)
		
	end do 
	
	
	
	! do jj = 1, face_num
		! if (centriod(1,jj) >= base_x) then
			! TG_up_avg = TG_up_avg + TG(jj)* centriod(4,jj)
		! endif
	! end do
	

	
	! do ww = 1, 16
		! write(*,*) 'the location is', ww
		! write(*,*) 'the Temperature Gradient is: ', Tg_local(ww)
		! write(*,*) 'the Wall Shear force is: ', ss_local(ww)
		! write(*,*) 'the Area is: ', Area_local(ww)
	! end do

	! write(*,*) 'the Total TG*A is: ', Tg_avg
	! write(*,*) 'the Total Area is: ', Area
	! write(*,*) 'the wall stress (velocity part) is: ', ss_avg
	! write(*,*) 'the UP TG*A is: ', TG_up_avg
	! write(*,*) 'the wall stress (pressure part) is: ', ssp_avg
	
	write(*,*) 'the Total Area is: ', Area
	
	write(*,*) 'Pfx is: ', Pfx_avg
	write(*,*) 'Pfy is: ', Pfy_avg
	write(*,*) 'Pfz is: ', Pfz_avg
	
	write(*,*) 'Vfx is: ', Vfx_avg
	write(*,*) 'Vfy is: ', Vfy_avg
	write(*,*) 'Vfz is: ', Vfz_avg
	
	write(*,*) 'TG is: ', TG_avg
	write(*,*) 'Tsurf is: ', TS_avg/Area
	
	
	!*==================================output=============================================*!
	! output Surface data 
	
	open(1, file = 'Surface_data2.csv', status='replace')  
	write(1,*) "X, Y, Z, Shear_stress, Psurf, Tsurf, Heat_flux"
    do xx = 1,face_num  
       write(1,*) centriod(1,xx),",",centriod(2,xx),",",centriod(3,xx),",",&
	              Shear_stress(xx),",",Psurf(xx),",",Tsurf(xx),",",TG(xx)
    end do  
    close(1) 
	
	!*==================================output=============================================*!
	
	
	
end program stl_detail