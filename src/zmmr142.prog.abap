*&---------------------------------------------------------------------*
*& Report  ZMMR142
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zmmr142.

PARAMETERS: ck_01 RADIOBUTTON GROUP rad1 DEFAULT 'X'.
PARAMETERS: ck_02 RADIOBUTTON GROUP rad1.
PARAMETERS: ck_03 RADIOBUTTON GROUP rad1.
PARAMETERS: pchaven TYPE zib_nfe_dist_ter-chave_nfe.

*-CS2020000990 - 07.01.2021 - inicio
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
    IF screen-name = 'CK_02' OR
       screen-name = 'CK_03'.
      AUTHORITY-CHECK OBJECT 'ZMMR142'
                          ID 'ZMMR142'
                       FIELD '01'.
      IF sy-subrc <> 0.
        screen-input  = 0.
*       screen-active = 0.
*       screen-invisible = 1.
      ENDIF.
    ENDIF.
    MODIFY SCREEN.
  ENDLOOP.
*-CS2020000990 - 07.01.2021 - fim

START-OF-SELECTION.

  DATA: obj_nfe TYPE REF TO zcl_nfe_inbound.

  DATA r_migo TYPE  mkpf.

  CREATE OBJECT obj_nfe.

  TRY .
      obj_nfe->zif_cadastro~set_registro( i_id_registro = pchaven ).
      CASE abap_true.
        WHEN ck_01.
          "Nova Solicitação de MIRO
          "PERFORM  valida_migo.
          "IF r_migo IS NOT INITIAL.
            obj_nfe->nfe_sol_miro_softexpert( ).
*          ELSE.
*            MESSAGE 'Não permitido criar SM! MIGO não encontrado.'   TYPE 'E'.
*            EXIT.
*          ENDIF.
        WHEN ck_02.
          "Cancelar Solicitação de MIRO
          obj_nfe->nfe_sol_miro_softexpert( i_estornar = 'E' ).
        WHEN ck_03.
          "Anexar MIRO/FISCAL em uma Solicitação
          obj_nfe->nfe_sol_anexa_miro_softexpert( ).
      ENDCASE.
      obj_nfe->zif_cadastro~limpar_registro( ).
      CLEAR: obj_nfe .
    CATCH cx_root INTO DATA(oexcp).
      DATA(i_texto) = oexcp->if_message~get_longtext( ).

      DATA: lc_texto TYPE c LENGTH 200.
      lc_texto = i_texto.
      sy-msgv1 = lc_texto+000(50).
      sy-msgv2 = lc_texto+050(50).
      sy-msgv3 = lc_texto+100(50).
      sy-msgv4 = lc_texto+150(50).

      MESSAGE ID zcx_nfe_inbound_exception=>zcx_erro_geral-msgid
         TYPE 'S'
       NUMBER zcx_nfe_inbound_exception=>zcx_erro_geral-msgno
         WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4
       DISPLAY LIKE 'E'.

  ENDTRY.


*FORM valida_migo.
*
*
*  DATA: lc_xblnr TYPE xblnr1,
*        l_serie  TYPE c LENGTH 3,
*        it_migo  TYPE TABLE OF mkpf.
*
*
*  SELECT * FROM zib_nfe_dist_ter INTO TABLE @DATA(it_zib_nfe_dist_ter)
*    WHERE chave_nfe EQ @pchaven.
*
*
*  CLEAR: r_migo.
*
*  READ TABLE it_zib_nfe_dist_ter INTO DATA(wa_item) INDEX 1.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wa_item-numero
*    IMPORTING
*      output = lc_xblnr.
*
*  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*    EXPORTING
*      input  = wa_item-serie
*    IMPORTING
*      output = l_serie.
*
*  CONCATENATE lc_xblnr l_serie INTO lc_xblnr SEPARATED BY '-'.
*
*
*  "Buscar por Vinculo de NF-e
*  SELECT SINGLE * INTO @r_migo
*    FROM mkpf AS m
*   WHERE m~zchave_nfe EQ @wa_item-chave_nfe
*     AND m~xblnr      EQ @lc_xblnr
*     AND NOT EXISTS ( SELECT * FROM mseg AS e WHERE e~smbln EQ m~mblnr AND e~sjahr EQ m~mjahr ).
*
*
*  TRY.
*      EXEC SQL.
*        OPEN SQL_MIRO FOR
*          SELECT M.MBLNR, M.MJAHR
*            FROM SAPHANADB.MKPF M
*           WHERE M.MANDT = :SY-MANDT
*             AND M.XBLNR = :LC_XBLNR
*             AND M.BLDAT >= :WA_ITEM-DT_EMISSAO
*             AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LIFNR = :WA_ITEM-P_EMISSOR AND E.SMBLN = ' ' )
*             AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
*      ENDEXEC.
*    CATCH cx_sy_native_sql_error INTO DATA(exc_ref).
*      DATA(error_text) = exc_ref->get_text( ).
*      MESSAGE error_text TYPE 'E'.
*  ENDTRY.
*
*  DO.
*    EXEC SQL.
*      FETCH NEXT SQL_MIRO INTO
*      :R_MIGO-MBLNR,
*      :R_MIGO-MJAHR
*    ENDEXEC.
*    IF sy-subrc <> 0.
*      EXIT.
*    ELSE.
*      APPEND r_migo TO it_migo.
*    ENDIF.
*  ENDDO.
*
*  EXEC SQL.
*    CLOSE SQL_MIRO
*  ENDEXEC.
*
*  IF it_migo[] IS INITIAL.
*
*    TRY.
*        EXEC SQL.
*          OPEN SQL_MIRO_2 FOR
*            SELECT M.MBLNR, M.MJAHR
*              FROM SAPHANADB.MKPF M
*             WHERE M.MANDT  = :SY-MANDT
*               AND M.XBLNR  = :LC_XBLNR
*               AND M.BLDAT >= :WA_ITEM-DT_EMISSAO
*               AND EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.MBLNR = M.MBLNR AND E.MJAHR = M.MJAHR AND E.LLIEF = :WA_ITEM-P_EMISSOR AND E.SMBLN = ' ' )
*               AND NOT EXISTS ( SELECT * FROM SAPHANADB.MSEG E WHERE E.SMBLN = M.MBLNR AND E.SJAHR = M.MJAHR )
*        ENDEXEC.
*      CATCH cx_sy_native_sql_error INTO exc_ref.
*        error_text = exc_ref->get_text( ).
*        MESSAGE error_text TYPE 'E'.
*    ENDTRY.
*
*    DO.
*      EXEC SQL.
*        FETCH NEXT SQL_MIRO_2 INTO
*        :R_MIGO-MBLNR,
*        :R_MIGO-MJAHR
*      ENDEXEC.
*      IF sy-subrc <> 0.
*        EXIT.
*      ELSE.
*        APPEND r_migo TO it_migo.
*      ENDIF.
*    ENDDO.
*
*    EXEC SQL.
*      CLOSE SQL_MIRO_2
*    ENDEXEC.
*
*    IF it_migo[] IS NOT INITIAL.
*      sy-subrc = 0.
*    ENDIF.
*
*  ELSE.
*    sy-subrc = 0.
*  ENDIF.
*
*  IF sy-subrc IS INITIAL.
*    READ TABLE it_migo INDEX 1 INTO r_migo.
*  ENDIF.
*
*
*ENDFORM.
