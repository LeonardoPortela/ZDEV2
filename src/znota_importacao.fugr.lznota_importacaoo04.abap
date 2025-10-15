*----------------------------------------------------------------------*
***INCLUDE LZNOTA_IMPORTACAOO04 .
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2004_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2004_exit INPUT.

  CASE ok_code_ad.
    WHEN ok_di_cancela.
      CLEAR: ok_di_altera, znota_import_ad.
      PERFORM pesquisa_di_ad USING vg_docnum vg_itmnum vg_itdidoc.
      IF NOT it_znota_import_ad[] IS INITIAL.
        READ TABLE it_znota_import_ad INTO znota_import_ad INDEX 1.
        LEAVE TO SCREEN 2004.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN ok_di_sair.
      LEAVE TO SCREEN 0.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2004_EXIT  INPUT


*&---------------------------------------------------------------------*
*&      Form  PESQUISA_DI_AD
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM pesquisa_di_ad  USING    p_docnum  TYPE j_1bdocnum
                              p_itmnum  TYPE j_1bitmnum
                              p_itdidoc TYPE j_1bitmnum.

  SELECT * INTO TABLE it_znota_import_ad
    FROM znota_import_ad
   WHERE docnum  EQ p_docnum
     AND itmnum  EQ p_itmnum
     AND itdidoc EQ p_itdidoc.

ENDFORM.                    " PESQUISA_DI_AD

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_2004  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_2004 INPUT.

  CASE ok_code_ad.
    WHEN ok_di_inserir.
      MOVE-CORRESPONDING znota_import TO znota_import_ad.
      znota_import_ad-cfabricante = znota_import-cexportador.
      ok_di_altera = c_x.
    WHEN ok_di_editar .
      ok_di_altera = c_x.
    WHEN ok_di_excluir.
      DELETE FROM znota_import_ad
       WHERE docnum    EQ znota_import_ad-docnum
         AND itmnum    EQ znota_import_ad-itmnum
         AND itdidoc   EQ znota_import_ad-itdidoc
         AND nr_adicao EQ znota_import_ad-nr_adicao.
      COMMIT  WORK.
      PERFORM pesquisa_di_ad USING vg_docnum vg_itmnum vg_itdidoc.
      IF NOT it_znota_import_ad[] IS INITIAL.
        READ TABLE it_znota_import_ad INTO znota_import_ad INDEX 1.
      ELSE.
        LEAVE TO SCREEN 0.
      ENDIF.
    WHEN ok_di_gravar.
      PERFORM f_valida_drawback USING znota_import_ad-docnum znota_import_ad-itmnum.
      MODIFY znota_import_ad.
      COMMIT WORK.
      PERFORM pesquisa_di_ad USING vg_docnum vg_itmnum vg_itdidoc.
      ok_di_altera = space.
  ENDCASE.

ENDMODULE.                 " USER_COMMAND_2004  INPUT

*&---------------------------------------------------------------------*
*&      Module  STATUS_2004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_2004 OUTPUT.

  CLEAR: it_fcode_di.

  CASE ok_di_altera.
    WHEN space.
      wa_fcode_di = ok_di_gravar.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_cancela.
      APPEND wa_fcode_di TO it_fcode_di.
    WHEN c_x.
      wa_fcode_di = ok_di_inserir.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_editar.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_excluir.
      APPEND wa_fcode_di TO it_fcode_di.
      wa_fcode_di = ok_di_sair.
      APPEND wa_fcode_di TO it_fcode_di.
  ENDCASE.

  SET PF-STATUS 'PFDI' EXCLUDING it_fcode_di.
  SET TITLEBAR  'TLDI'.

ENDMODULE.                 " STATUS_2004  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  VISIBILIDADE_2004  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE visibilidade_2004 OUTPUT.

  LOOP AT SCREEN.
    IF ok_di_altera IS INITIAL.
      screen-output = '1'.
      screen-input  = '0'.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.

  PERFORM f_habilita_drawback.

ENDMODULE.                 " VISIBILIDADE_2004  OUTPUT
*&---------------------------------------------------------------------*
*&      Form  F_VALIDA_DRAWBACK
*&---------------------------------------------------------------------*
FORM f_valida_drawback  USING    p_docnum
                                 p_itmnum.

  DATA: wa_cfop_drawback LIKE LINE OF gra_cfop_drawback.

  "Carrega SET de CFOP para drawback.
  IF gra_cfop_drawback[] IS INITIAL.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MAGGI_DRAWBACK'
        table         = 'J_1BNFLIN'
        class         = '0000'
        fieldname     = 'CFOP'
      TABLES
        set_values    = it_set_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.
      LOOP AT it_set_values INTO DATA(lwa_set_values).

        TRANSLATE lwa_set_values-from USING '/ '.
        TRANSLATE lwa_set_values-to USING '/ '.
        CONDENSE lwa_set_values-from NO-GAPS.
        CONDENSE lwa_set_values-to NO-GAPS.

        IF lwa_set_values-to IS NOT INITIAL.
          wa_cfop_drawback      = 'IBT'.
          wa_cfop_drawback-low  = lwa_set_values-from.
          wa_cfop_drawback-high = lwa_set_values-to.
          APPEND wa_cfop_drawback TO gra_cfop_drawback.
        ELSE.
          wa_cfop_drawback      = 'IEQ'.
          wa_cfop_drawback-low  = lwa_set_values-from.
          APPEND wa_cfop_drawback TO gra_cfop_drawback.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO @DATA(lwa_itens)
     FROM j_1bnflin
     WHERE docnum EQ @p_docnum
       AND itmnum EQ @p_itmnum.
  IF lwa_itens-cfop IN gra_cfop_drawback.
    IF znota_import_ad-nr_drawback IS INITIAL.
      MESSAGE ID 'ZSIMETRYA' TYPE 'I' NUMBER '023' WITH 'Preencher número de drawback'.
      LEAVE SCREEN.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HABILITA_DRAWBACK
*&---------------------------------------------------------------------*
FORM f_habilita_drawback .

  DATA: wa_cfop_drawback LIKE LINE OF gra_cfop_drawback.

  "Carrega SET de CFOP para drawback.
  IF gra_cfop_drawback[] IS INITIAL.

    CALL FUNCTION 'G_SET_GET_ALL_VALUES'
      EXPORTING
        setnr         = 'MAGGI_DRAWBACK'
        table         = 'J_1BNFLIN'
        class         = '0000'
        fieldname     = 'CFOP'
      TABLES
        set_values    = it_set_values
      EXCEPTIONS
        set_not_found = 1
        OTHERS        = 2.
    IF sy-subrc IS INITIAL.
      LOOP AT it_set_values INTO DATA(lwa_set_values).

        TRANSLATE lwa_set_values-from USING '/ '.
        TRANSLATE lwa_set_values-to USING '/ '.
        CONDENSE lwa_set_values-from NO-GAPS.
        CONDENSE lwa_set_values-to NO-GAPS.

        IF lwa_set_values-to IS NOT INITIAL.
          wa_cfop_drawback      = 'IBT'.
          wa_cfop_drawback-low  = lwa_set_values-from.
          wa_cfop_drawback-high = lwa_set_values-to.
          APPEND wa_cfop_drawback TO gra_cfop_drawback.
        ELSE.
          wa_cfop_drawback      = 'IEQ'.
          wa_cfop_drawback-low  = lwa_set_values-from.
          APPEND wa_cfop_drawback TO gra_cfop_drawback.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDIF.

  SELECT SINGLE * INTO @DATA(lwa_itens)
     FROM j_1bnflin
     WHERE docnum EQ @znota_import_ad-docnum
       AND itmnum EQ @znota_import_ad-itmnum.
  IF lwa_itens-cfop NOT IN gra_cfop_drawback.
    LOOP AT SCREEN.
      IF screen-name = 'ZNOTA_IMPORT_AD-NR_DRAWBACK'.
        screen-output = '1'.
        screen-input  = '0'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ELSE.
    LOOP AT SCREEN.
      IF screen-name = 'ZNOTA_IMPORT_AD-NR_DRAWBACK'.
        screen-input  = '1'.
        MODIFY SCREEN.
      ENDIF.
    ENDLOOP.
  ENDIF.
ENDFORM.
