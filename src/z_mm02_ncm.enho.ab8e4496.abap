"Name: \FU:MATERIAL_UPDATE_ALL\SE:BEGIN\EI
ENHANCEMENT 0 Z_MM02_NCM.
*
  DATA: TL_SETLEAF TYPE TABLE OF SETLEAF WITH HEADER LINE.
  DATA: TL_PARAMETROS TYPE USTYP_T_PARAMETERS,
        WL_PARAMETROS TYPE USTYP_PARAMETERS.

  if sy-tcode = 'MM02'.
      IF omarc-steuc ne wmarc-steuc.
        SELECT *
        FROM SETLEAF
        INTO TABLE TL_SETLEAF
         WHERE SETNAME EQ 'MAGGI_MM01_ABA_NCM'
           AND VALFROM EQ omara-MATKL.
        IF sy-subrc = 0.
           CALL FUNCTION 'SUSR_USER_PARAMETERS_GET'
           EXPORTING
             USER_NAME           = SY-UNAME
           TABLES
             USER_PARAMETERS     = TL_PARAMETROS
           EXCEPTIONS
             USER_NAME_NOT_EXIST = 1
             OTHERS              = 2.
         IF SY-SUBRC <> 0.
         ENDIF.

         READ TABLE TL_PARAMETROS INTO WL_PARAMETROS
          WITH KEY PARID = 'ZMAT_NCM'.

         IF SY-SUBRC NE 0.
           IF WL_PARAMETROS-PARVA NE '*'.
                message e000(Z01) WITH 'Para este grupo de mercadoria, o usuario,'
                                        sy-uname
                                        'não tem autorização no parametro ZMAT_NCM'.
           Endif.
         ENDIF.
       ENDIF.
      ENDIF.
  ENDIF.
ENDENHANCEMENT.
