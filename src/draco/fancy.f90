module fancy
    implicit none

    public

contains
    
    subroutine header(unit, pr)
        integer, intent(in) :: unit
        integer, intent(in) :: pr

        if (pr<0) return
                                  
        if (pr>0) then 
            write(unit,'(5x,a)')'', &
            '                    ___  ___  ___  _________                 ', &
            '                   / _ \/ _ \/ _ |/ ___/ __ \                ', &
            ' -----------------/ // / , _/ _  / /__/ /_/ /--------------  ', &
            '|                /____/_/|_/_/ |_\___/\____/                |', &
            '|                                                           |'
        else 
            write(unit,'(5x,a)')'', &
            ' -----------------------------------------------------------  ', &    
            '|                   =====================                   |', &     
            '|                           DRACO                           |'
        end if
        
        write(unit,'(5x,a)') &     
        '|                   =====================                   |', &     
        '|                     C. Plett, M.Stahn                     |', &     
        '|          Mulliken Center for Theoretical Chemistry        |', &     
        '|                    University of Bonn                     |', &     
        '|             DOI: 10.1021/acs.jpclett.3c03551              |', &     
        ' -----------------------------------------------------------  '
        if (pr>0) write(unit,'(20x,a)')'', &
        '        This is DRACO.  ', &
        '      ⢀⣠⣤⣶⣶⡞⡀⣤⣬⣴⠀⠀⢳⣶⣶⣤⣄⡀⠀⠀⠀⠀⠀ ',& 
        '⠀⠀⠀⠀⣠⣾⣿⣿⣿⣿⡇⠀⢸⣿⠿⣿⡇⠀⠀⠸⣿⣿⣿⣿⣷⣦⡀⠀⠀⠀',&
        '⠀⠀⢠⡾⣫⣿⣻⣿⣽⣿⡇⠀⠈⢿⣧⡝⠟⠀⠀⢸⣿⣿⣿⣿⣿⣟⢷⣄⠀⠀',&
        '⠀⢠⣯⡾⢿⣿⣿⡿⣿⣿⣿⣆⣠⣶⣿⣿⣷⣄⣰⣿⣿⣿⣿⣿⣿⣿⢷⣽⣄⠀',&
        '⢠⣿⢋⠴⠋⣽⠋⡸⢱⣯⡿⣿⠏⣡⣿⣽⡏⠹⣿⣿⣿⡎⢣⠙⢿⡙⠳⡙⢿⠄',&
        '⣰⢣⣃⠀⠊⠀⠀⠁⠘⠏⠁⠁⠸⣶⣿⡿⢿⡄⠈⠀⠁⠃⠈⠂⠀⠑⠠⣈⡈⣧',&
        '⡏⡘⠁⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⢀⡥⢄⢸⡇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢳⢸',&
        '⠇⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠸⣄⣸⠟⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈⢨',&
        '⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡴⠋⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠈',&
        '⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⡳⣶⣄⡀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀⠀', &
        ''
        call draco_version(unit)
        call disclaimer(unit)
                                                   
    end subroutine header

    subroutine draco_version(unit)                                                                                                                                                                  
                                                                                                                                                                                               
        implicit none                                                                                                                                                                               
        integer,intent(in) :: unit                                                                                                                                                                 
        !! I/O unit                                                                                                                                                                              
                                                                                                                                                                                               
        include 'draco_version.fh'                                                                                                                                                                    
        write(unit,'(3x,"*",*(1x,a))') &                                                                                                                                                           
        & "DRACO version", version, "compiled by", author, "on", date                                                                                                                              
        write(unit,'(a)')                                                                                                                                                                          
                                                                                                                                                                                               
    end subroutine draco_version 

    subroutine generic_header(iunit,string,width,offset)                                                                                                                                           
        implicit none                                                                                                                                                                                  
        integer,intent(in) :: iunit                                                                                                                                                                    
        integer,intent(in) :: offset                                                                                                                                                                   
        integer,intent(in) :: width                                                                                                                                                                    
        character(len=*),intent(in) :: string                                                                                                                                                          
        character(len=width) :: dum1,dum2                                                                                                                                                              
        character(len=2*width) :: outstring                                                                                                                                                            
        character(len=width) :: formatstr                                                                                                                                                              
        integer :: strlen,ifront,iback                                                                                                                                                                 
        strlen = len(string)                                                                                                                                                                           
        ifront = (width - strlen)/2                                                                                                                                                                    
        iback  = width - ifront - strlen                                                                                                                                                               
        write(dum1,*) width                                                                                                                                                                            
        write(dum2,*) offset                                                                                                                                                                           
        write(formatstr,'(i0,"x,a,",i0,"x")') ifront,iback                                                                                                                                             
        write(outstring,'("|",'//formatstr//',"|")') string                                                                                                                                            
        write(iunit,'('//dum2//'x,1x,'//dum1//'("-"),1x)')                                                                                                                                             
        write(iunit,'('//dum2//'x,a)') trim(outstring)                                                                                                                                                 
        write(iunit,'('//dum2//'x,1x,'//dum1//'("-"),1x)')                                                                                                                                             
    end subroutine generic_header

    subroutine disclaimer(iunit)                                                                                                                                                                                
        integer,intent(in) :: iunit                                                                                                                                                                                
        write(iunit,'(3x,a)') &                                                                                                                                                                                    
           !< < < < < < < < < < < < < < < > > > > > > > > > > > > > > > >!                                                                                                                                         
           "DRACO is free software: you can redistribute it and/or modify it under", &                                                                                                                               
           "the terms of the GNU Lesser General Public License as published by", &                                                                                                                                 
           "the Free Software Foundation, either version 3 of the License, or", &                                                                                                                                  
           "(at your option) any later version.", &                                                                                                                                                                
           "", &                                                                                                                                                                                                   
           "DRACO is distributed in the hope that it will be useful,", &                                                                                                                                             
           "but WITHOUT ANY WARRANTY; without even the implied warranty of", &                                                                                                                                     
           "MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the", &                                                                                                                                      
           "GNU Lesser General Public License for more details.", &                                                                                                                                                
           ""                                                                                                                                                                                                      
    end subroutine disclaimer

end module fancy
