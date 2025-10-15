"Name: \PR:SAPLSUID_MAINTENANCE\TY:LCL_MAINT_MAIN\ME:C_UC_MAINT_SCREEN\SE:BEGIN\EI
ENHANCEMENT 0 ZCL_MAINT_MAIN.
*
* RJF - Ini - Correções baixa de usuário e criação de usuário SAP - Auditoria #136523
*.. command from maintenenace screen

    CASE M_OKCODE.
      WHEN 'UPD' OR 'N' OR 'BACK'.
        DATA: LV_CPF_NR                TYPE PBR_CPFNR,
              LV_CPF_NRF               TYPE PBR_CPFNR,
              MESSAGES                 TYPE TABLE OF ADDR_ERROR,
              LV_NUMBER_CANONICAL_LONG TYPE CHAR100,
*              WORST_ERROR              type TABLE OF ADDR_ERROR-MSG_TYPE,
              LV_PERNR                 TYPE PERSNO,
              LT_RETURN                TYPE TABLE OF BAPIRET2, "US #169472 - MMSILVA - 18.03.2025
              LS_LOGONDATA             TYPE BAPILOGOND. "US #169472 - MMSILVA - 18.03.2025


        CONSTANTS: C_ERROR   TYPE AD_RETCODE VALUE 'E'.

        FIELD-SYMBOLS: <FS_USTYP>     TYPE ANY,
                       <FS_BNAME>     TYPE ANY,
                       <FS_FAX_NUMER> TYPE ANY,
                       <FS_FLOOR>     TYPE ANY,
                       <FS_GLTGB>     TYPE ANY. "US #169472 - MMSILVA - 18.03.2025

        LOOP AT SCREEN.
          IF SCREEN-NAME EQ 'SUID_ST_NODE_LOGONDATA-GLTGB'.
            BREAK-POINT.
          ENDIF.

        ENDLOOP.


        ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_BNAME-BNAME')               TO <FS_BNAME>.
        ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_COMM_DATA-FAX_NUMBER') TO <FS_FAX_NUMER>.
        ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_LOGONDATA-USTYP')      TO <FS_USTYP>.
        ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_WORKPLACE-FLOOR')      TO <FS_FLOOR>.
        ASSIGN ('(SAPLSUID_MAINTENANCE)SUID_ST_NODE_LOGONDATA-GLTGB')      TO <FS_GLTGB>. "US #169472 - MMSILVA - 18.03.2025

        IF  <FS_USTYP> IS ASSIGNED AND <FS_USTYP> IS NOT INITIAL.

          IF <FS_USTYP> EQ 'A'.
            DATA(LV_VALID) = ABAP_TRUE.
          ENDIF.

        ELSE.

          IF  <FS_BNAME> IS ASSIGNED AND <FS_BNAME> IS NOT INITIAL.
            SELECT USTYP FROM USR02
              UP TO 1 ROWS
              INTO @DATA(LV_USTYP)
              WHERE BNAME EQ @<FS_BNAME>.
            ENDSELECT.
            IF SY-SUBRC IS INITIAL AND LV_USTYP EQ 'A'.
              LV_VALID = ABAP_TRUE.
            ELSE.
              CLEAR LV_VALID.
            ENDIF.
          ENDIF.
        ENDIF.

        IF LV_VALID IS NOT INITIAL.
          IF  <FS_FLOOR> IS ASSIGNED." AND <fs_floor> IS NOT INITIAL.
            "US #169472 - MMSILVA - 18.03.2025 - Inicio
*            IF (   <fs_floor> EQ 'AMAGGI_INT'  ) OR
*               (   <fs_floor> EQ 'CONSULTOR' ) OR
*               (   <fs_floor> EQ 'MAPRENDIZ' ) OR
*               (   <fs_floor> EQ 'CONSELHO'  ) OR
*               (   <fs_floor> EQ 'OUTROS'    ).
*              lv_valid = abap_false.
*            ELSE.
*              lv_valid = abap_true.
*            ENDIF.

            SELECT SINGLE * FROM TVARVC INTO @DATA(LT_TVARVC) WHERE NAME EQ 'Z_SU01_ANDAR' AND LOW EQ @<FS_FLOOR>.
            IF SY-SUBRC IS INITIAL.
              LV_VALID = ABAP_FALSE.
            ELSE.
              LV_VALID = ABAP_TRUE.
            ENDIF.
            "US #169472 - MMSILVA - 18.03.2025 - Fim
          ENDIF.
        ENDIF.

        IF LV_VALID IS NOT INITIAL.
          IF <FS_FAX_NUMER> IS NOT INITIAL.
            WRITE <FS_FAX_NUMER> USING EDIT MASK
                  '___.___.___-__'
                  TO LV_CPF_NR.

*Ir na tabela PA0465 com CPF_NR = FAX
*Pegar PERNR
*Se encontrar, então pode salvar o registro.
            SELECT PERNR CPF_NR
              UP TO 1 ROWS
              FROM PA0465
              INTO ( LV_PERNR, LV_CPF_NRF )
              WHERE CPF_NR EQ LV_CPF_NR.
            ENDSELECT.
            IF SY-SUBRC IS NOT INITIAL AND LV_CPF_NRF IS INITIAL.
*Se não encontrar, mensagem : "Não foi encontrado empregado com o CPF informado"

*              PERFORM error5
*                      IN PROGRAM saplsza0
*                      TABLES messages
*                      USING 'ZCADASTRO'
*                            '001'
*                            lv_number_canonical_long
*                            space space space
*                            space
*                            'TELNUMBER'
*                            0.
*              PERFORM set_new_returncode
*                      IN PROGRAM saplsza0
*                      USING    c_error.
**              CHANGING worst_error.

              MESSAGE E001(ZCADASTRO).

            ENDIF.

          ELSEIF SY-SUBRC IS INITIAL AND LV_CPF_NRF IS NOT INITIAL.

*            PERFORM error5
*                    IN PROGRAM saplsza0
*                    TABLES messages
*                    USING 'ZCADASTRO'
*                          '003'
*                          lv_number_canonical_long
*                          space space space
*                          space
*                          'TELNUMBER'
*                          0.
*            PERFORM set_new_returncode
*                    IN PROGRAM saplsza0
*                    USING    c_error.
**            CHANGING worst_error.

            MESSAGE E003(ZCADASTRO).

          ELSEIF <FS_FAX_NUMER> IS INITIAL. " e tipo user...

            MESSAGE E002(ZCADASTRO).
*            PERFORM error5
*                    IN PROGRAM saplsza0
*                    TABLES messages
*                    USING 'ZCADASTRO'
*                          '002'
*                          lv_number_canonical_long
*                          space space space
*                          space
*                          'TELNUMBER'
*                          0.
**            PERFORM set_new_returncode
**                    IN PROGRAM saplsza0
**                    USING    c_error
**                    CHANGING worst_error.
          ENDIF.
        ENDIF.

        "US #169472 - MMSILVA - 18.03.2025 - Inicio
        SELECT SINGLE * FROM USR02 INTO @DATA(LS_USR02) WHERE BNAME EQ @<FS_BNAME>.

        CALL FUNCTION 'BAPI_USER_GET_DETAIL'
          EXPORTING
            USERNAME  = <FS_BNAME>
          IMPORTING
            LOGONDATA = LS_LOGONDATA " Importando os dados de logon
          TABLES
            RETURN    = LT_RETURN.

        IF LS_LOGONDATA IS NOT INITIAL.
          IF ( <FS_FLOOR> IS NOT INITIAL AND 'TERCEIROS_CONSULTOR' CS <FS_FLOOR> ) AND ( LS_LOGONDATA-GLTGB IS INITIAL ).
            MESSAGE E004(ZCADASTRO).
          ELSEIF ( <FS_FLOOR> IS NOT INITIAL AND 'TERCEIROS_CONSULTOR' CS <FS_FLOOR> ) AND ( LS_LOGONDATA-GLTGB LT SY-DATUM ).
            MESSAGE E005(ZCADASTRO).
          ENDIF.
        ELSEIF ( <FS_GLTGB> EQ '00000000' OR <FS_GLTGB> LT SY-DATUM ) AND LS_USR02-GLTGB EQ '00000000' AND <FS_FLOOR> IS NOT INITIAL AND 'TERCEIROS_CONSULTOR' CS <FS_FLOOR>.
          IF <FS_GLTGB> EQ '00000000'.
            MESSAGE E004(ZCADASTRO).
          ELSEIF <FS_GLTGB> LT SY-DATUM.
            MESSAGE E005(ZCADASTRO).
          ENDIF.
        ENDIF.
        "US #169472 - MMSILVA - 18.03.2025 - Fim
    ENDCASE.
* RJF - Fim - Correções baixa de usuário e criação de usuário SAP - Auditoria #136523

ENDENHANCEMENT.
