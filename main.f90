PROGRAM main 

! Variáveis lidas no input  
  CHARACTER(LEN=100) :: input_file, output_file              
  REAL(8) :: Lx,Ly,Lz 
  INTEGER :: natoms, nframes, status_open, i, j, aux, control = 0
  REAL(8), DIMENSION(:,:,:), ALLOCATABLE :: atom_pos        ! atoms is the CoM position
  INTEGER, DIMENSION(:), ALLOCATABLE :: timestep

  ! reading input file
  READ(*,*);  READ(*,*) input_file 
  READ(*,*);  READ(*,*) output_file
  READ(*,*);  READ(*,*) natoms  
  READ(*,*);  READ(*,*) Lx,Ly,Lz 
  
  nframes = 0  
  
  OPEN(unit=1,file=input_file,status="old",iostat = status_open ) 
  IF (status_open > 0) STOP "error opening input_file"
  
  DO WHILE (control == 0)
         nframes = nframes + 1 !getting number of lines
         READ (1 , * , iostat= control) 
  END DO
  
  REWIND 1
  
  OPEN(unit=2,file=output_file,status="replace",iostat = status_open ) 
  IF (status_open > 0) STOP "error opening input_file"
  
  READ(1,*); READ(1,*); READ(1,*) ! comment this for erika's file
  nframes = (nframes - 3) / (natoms + 1) ! comment this for erika's file
  !nframes = (nframes) / (natoms + 1) !érika

  ALLOCATE(atom_pos(natoms,3,nframes))
  ALLOCATE(timestep(nframes))
  
  DO i = 1,nframes
    READ(1,*) timestep(i),natoms ! comment this for erika's file
    !READ(1,*) timestep(i), aux ! uncomment this for erika's file
    WRITE(*,*) "Reading timestep", timestep(i)    
    DO j = 1, natoms    
      READ(1,*) aux, atom_pos(j,1,i), atom_pos(j,2,i), atom_pos(j,3,i) ! comment this for erika's file
      !READ(1,*) atom_pos(j,1,i), atom_pos(j,2,i), atom_pos(j,3,i)   ! uncomment this for erika's file    
    END DO      
  END DO 
  
  WRITE(*,*);WRITE(*,*);WRITE(*,*);WRITE(*,*)
  
  DO i = 1, nframes
    WRITE(*,*) "Writing timestep", timestep(i)  
    WRITE (2,'("ITEM: TIMESTEP")')
    WRITE (2,'(i20)') timestep(i)
    WRITE (2,'("ITEM: NUMBER OF ATOMS")')
    WRITE (2,'(i10)') natoms
    WRITE (2,'("ITEM: BOX BOUNDS pp pp pp")')  
    WRITE (2,'(f11.5,2x,f11.5)') -Lx/2 , Lx/2 
    WRITE (2,'(f11.5,2x,f11.5)') -Ly/2 , Ly/2
    WRITE (2,'(f11.5,2x,f11.5)') -Lz/2 , Lz/2
    WRITE (2,'("ITEM: ATOMS id mol type x y z")')

    DO j = 1,natoms
                  !atom_id , mol_id, atom_type, x,y,z  
      WRITE (2,*) j,j,1,atom_pos(j,1,i), atom_pos(j,2,i), atom_pos(j,3,i) 
    END DO     
    
  END DO 
 
  CLOSE(2)
  CLOSE(1)
  
  DEALLOCATE(atom_pos,timestep)
  
END PROGRAM main
