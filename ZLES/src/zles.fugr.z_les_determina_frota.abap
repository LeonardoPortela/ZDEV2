FUNCTION Z_LES_DETERMINA_FROTA.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(WK_VTTK) TYPE  VTTK
*"  EXPORTING
*"     VALUE(ADD03) TYPE  VTTK_ADD03
*"----------------------------------------------------------------------

  DATA: WA_FILIAL_LOCAL  TYPE J_1BBRANCH,
        WA_FILIAL_AGENTE TYPE J_1BBRANCH,
        MSG_TEXTO        TYPE STRING,
        VG_COD_AGENTE    TYPE TDLNR,
        WA_CENTRO_REAL   TYPE ZSDT_DEPARA_CEN.

  VG_COD_AGENTE = WK_VTTK-TDLNR.

  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      INPUT  = VG_COD_AGENTE
    IMPORTING
      OUTPUT = VG_COD_AGENTE.

  IF STRLEN( VG_COD_AGENTE ) LE 4.

    SELECT SINGLE * INTO WA_FILIAL_LOCAL
      FROM J_1BBRANCH
      WHERE BRANCH EQ  WK_VTTK-TPLST.

    IF NOT SY-SUBRC IS INITIAL.
      SELECT SINGLE * INTO WA_CENTRO_REAL
        FROM ZSDT_DEPARA_CEN
       WHERE CENTROV_1 EQ WK_VTTK-TPLST.

      IF SY-SUBRC IS INITIAL.
        SELECT SINGLE * INTO WA_FILIAL_LOCAL
          FROM J_1BBRANCH
          WHERE BRANCH EQ  WA_CENTRO_REAL-CENTRO_REAL.
      ENDIF.
    ENDIF.

    IF SY-SUBRC IS INITIAL.

      WA_FILIAL_AGENTE-BRANCH = VG_COD_AGENTE.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          INPUT  = WA_FILIAL_AGENTE-BRANCH
        IMPORTING
          OUTPUT = WA_FILIAL_AGENTE-BRANCH.

      SELECT SINGLE * INTO WA_FILIAL_AGENTE
        FROM J_1BBRANCH
        WHERE BRANCH EQ  WA_FILIAL_AGENTE-BRANCH.

      IF ( SY-SUBRC EQ 0 ).
        CASE wk_vttk-VSART.
          WHEN: '03'.
            ADD03 = '0000000002'.
          WHEN OTHERS.
            ADD03 = '0000000001'.
        ENDCASE.
      ELSE.
        ADD03 = '0000000002'.
      ENDIF.


*      if sy-subrc is initial.
*
*        if wa_filial_agente-bukrs eq wa_filial_local-bukrs.
*          add03 = '0000000001'.
*        else.
*          add03 = '0000000002'.
*        endif.
*      else.
*        concatenate 'Agente de frete ' wk_vttk-tplst 'não encontrada como local de negócio!' into msg_texto.
*        message msg_texto type 'W'.
*      endif.


    ELSE.
      CONCATENATE 'Organização de transporte' WK_VTTK-TPLST 'não encontrada como local de negócio!' INTO MSG_TEXTO.
      MESSAGE MSG_TEXTO TYPE 'W'.
    ENDIF.

  ELSE.
    ADD03 = '0000000002'.
  ENDIF.

ENDFUNCTION.
