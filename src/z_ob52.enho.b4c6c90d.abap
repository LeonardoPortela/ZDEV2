"Name: \PR:SAPLSPO4\FO:P100_CODE\SE:BEGIN\EI
ENHANCEMENT 0 Z_OB52.
*
  DATA: TL_PARAMETROS        TYPE USTYP_T_PARAMETERS,
        WL_PARAMETROS        TYPE USTYP_PARAMETERS.

    if sy-tcode = 'OB52'.
        CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
        EXPORTING
          USER_NAME           = sy-UNAME
*         WITH_TEXT           =
        TABLES
          USER_PARAMETERS     = TL_PARAMETROS
        EXCEPTIONS
          USER_NAME_NOT_EXIST = 1
          OTHERS              = 2.
      IF SY-SUBRC <> 0.
      ENDIF.

      READ TABLE TL_PARAMETROS INTO WL_PARAMETROS
        WITH KEY PARID = 'ZOB52'.
      IF sy-subrc ne 0.
          if sy-langu = 'S'.
            MESSAGE 'No hay ningún parámetro de acceso' type 'E'.
          elseif sy-langu = 'E'.
            MESSAGE 'There is no parameter to access' type 'E'.
          else.
            MESSAGE 'Não existe parâmetro para acesso' type 'E'.
          endif.
      ENDIF.

      IF WL_PARAMETROS-PARVA ne '*'.
          if not ( WL_PARAMETROS-PARVA cs SVALD-VALUE ) or WL_PARAMETROS-PARVA is INITIAL or SVALD-VALUE is INITIAL.
             if sy-langu = 'S'.
               MESSAGE 'Empresa no permitió!' type 'E'.
             elseif sy-langu = 'E'.
               MESSAGE 'Company not allowed!' type 'E'.
             else.
               MESSAGE 'Empresa não permitida!' type 'E'.
             endif.
          endif.
      ENDIF.
  endif.


ENDENHANCEMENT.
