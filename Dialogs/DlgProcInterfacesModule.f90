module DlgProcInterfacesModule
    interface 
        function  VerticalParamsDlgProc(hwnd, mesg, wParam, lParam)
            !! DO NOT FORGET TO UPDATE THESE CALL PROCEDURES !!
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_VerticalParamsDlgProc@16' :: VerticalParamsDlgProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'VerticalParamsDlgProc' :: VerticalParamsDlgProc
            !DEC$ ENDIF
            use ifwinty
            integer(LRESULT) :: VerticalParamsDlgProc ! LRESULT
            integer(HANDLE)   hwnd 
            integer(UINT)     mesg  
            integer(UINT_PTR) wParam
            integer(LONG_PTR) lParam 
        end function 
    end interface
    interface 
        function  ModelControlDlgProc(hwnd, mesg, wParam, lParam)
            !! DO NOT FORGET TO UPDATE THESE CALL PROCEDURES !!
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_ModelControlDlgProc@16' :: ModelControlDlgProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'ModelControlDlgProc' :: ModelControlDlgProc
            !DEC$ ENDIF
            use ifwinty
            integer(LRESULT) :: ModelControlDlgProc ! LRESULT
            integer(HANDLE)   hwnd 
            integer(UINT)     mesg  
            integer(UINT_PTR) wParam
            integer(LONG_PTR) lParam 
        end function 
    end interface

    interface 
        function  SettingsDlgProc(hwnd, mesg, wParam, lParam)
            !! DO NOT FORGET TO UPDATE THESE CALL PROCEDURES !!
            !DEC$ IF DEFINED(_X86_)
            !DEC$ ATTRIBUTES STDCALL, ALIAS : '_SettingsDlgProc@16' :: SettingsDlgProc
            !DEC$ ELSE
            !DEC$ ATTRIBUTES STDCALL, ALIAS : 'SettingsDlgProc' :: SettingsDlgProc
            !DEC$ ENDIF
            use ifwinty
            integer(LRESULT) :: SettingsDlgProc ! LRESULT
            integer(HANDLE)   hwnd 
            integer(UINT)     mesg  
            integer(UINT_PTR) wParam
            integer(LONG_PTR) lParam 
        end function 
    end interface

    end module DlgProcInterfacesModule
