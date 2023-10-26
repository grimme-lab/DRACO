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

end module fancy