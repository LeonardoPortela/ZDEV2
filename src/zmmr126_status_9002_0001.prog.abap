*----------------------------------------------------------------------*
***INCLUDE ZMMR126_STATUS_9002.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Module  STATUS_9002  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE status_9002 OUTPUT.

  SET PF-STATUS 'PF9002'.
  SET TITLEBAR 'TL9002'.

  IF wa_add_nfe_9002-n55_chave_acesso IS NOT INITIAL.
    PERFORM buscar_indo_nota_digitada.
  ENDIF.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002_exit INPUT.
  wa_add_nfe_9002-ck_incluir = abap_false.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Form  BUSCAR_INDO_NOTA_DIGITADA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM buscar_indo_nota_digitada .

  DATA: qtd TYPE i,
        nfe TYPE REF TO zcl_nfe_inbound.

  qtd = strlen( wa_add_nfe_9002-n55_chave_acesso ).

  wa_add_nfe_9002-ck_incluir = abap_false.

  IF qtd NE 44.
    MESSAGE s034 WITH wa_add_nfe_9002-n55_chave_acesso.
    EXIT.
  ENDIF.

  TRY .
      CREATE OBJECT nfe
        EXPORTING
          i_chave_nfe = wa_add_nfe_9002-n55_chave_acesso.

      TRY .
          nfe->set_info_sap( ).
        CATCH zcx_nfe_inbound_exception.
        CATCH zcx_cadastro.
        CATCH zcx_pedido_compra_exception.
      ENDTRY.

      DATA(info_nota) = nfe->get_info_nota( ).
      nfe->free( ).
      CLEAR: nfe.
      wa_add_nfe_9002-branch     = info_nota-nfe_base-f_tomadora.
      wa_add_nfe_9002-bukrs      = info_nota-nfe_base-e_tomadora.
      wa_add_nfe_9002-docnum_nfe = info_nota-nfe_base-docnum_nfe.
      wa_add_nfe_9002-parid      = info_nota-nfe_base-p_emissor.
      SELECT SINGLE stcd3 INTO wa_add_nfe_9002-parid_ie
        FROM lfa1
       WHERE lifnr EQ info_nota-nfe_base-p_emissor.
      wa_add_nfe_9002-nftot      = info_nota-nfe_base-vl_total.
      wa_add_nfe_9002-dt_emissao = info_nota-nfe_base-dt_emissao.
      wa_add_nfe_9002-numero     = info_nota-nfe_base-numero.
      wa_add_nfe_9002-serie      = info_nota-nfe_base-serie.
      wa_add_nfe_9002-ntgew      = 0.

      LOOP AT info_nota-nfe_base-itens INTO DATA(wa_item).
        TRANSLATE wa_item-prod_und_comerci TO UPPER CASE.
        CASE zcl_str=>upper( CONV #( wa_item-prod_und_comerci ) )->get( ).
          WHEN 'KG'.
            ADD wa_item-prod_qtd_comerci TO wa_add_nfe_9002-ntgew.
          WHEN 'TO' OR 'TON'.
            wa_item-prod_qtd_comerci = wa_item-prod_qtd_comerci * 1000.
            ADD wa_item-prod_qtd_comerci TO wa_add_nfe_9002-ntgew.
        ENDCASE.
        wa_add_nfe_9002-cfop = wa_item-prod_cfop.
      ENDLOOP.

      SELECT SINGLE butxt INTO wa_add_nfe_9002-butxt
        FROM t001
       WHERE bukrs EQ info_nota-nfe_base-e_tomadora.

      SELECT SINGLE name INTO wa_add_nfe_9002-name
        FROM j_1bbranch
       WHERE bukrs EQ info_nota-nfe_base-e_tomadora
         AND branch EQ info_nota-nfe_base-f_tomadora.

      SELECT SINGLE name1 INTO wa_add_nfe_9002-name1
        FROM lfa1
       WHERE lifnr EQ wa_add_nfe_9002-parid.

      wa_add_nfe_9002-ck_incluir = abap_true.

    CATCH zcx_nfe_inbound_exception INTO DATA(ex_nfe_inbound_exception).
      IF nfe IS NOT INITIAL.
        nfe->free( ).
      ENDIF.
      ex_nfe_inbound_exception->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
    CATCH zcx_cadastro INTO DATA(ex_cadastro).
      IF nfe IS NOT INITIAL.
        nfe->free( ).
      ENDIF.
      ex_cadastro->published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
      IF ex_cadastro->msgid = 'ZNFE_DISTRI' AND ex_cadastro->msgno = 103.
        MESSAGE i035.
      ENDIF.
  ENDTRY.

  "WA_ADD_NFE_9002-.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_9002  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE user_command_9002 INPUT.

  CASE ok_code.
    WHEN 'CONFIRMAR'.
      CLEAR: ok_code.
      PERFORM buscar_indo_nota_digitada.

      IF ck_alterado_chave EQ abap_true.
        ck_alterado_chave = abap_false.
        EXIT.
      ENDIF.

      IF wa_add_nfe_9002-ck_incluir EQ abap_true.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ATRIBUI_INFO_CHAVE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE atribui_info_chave INPUT.
  ck_alterado_chave = abap_true.
ENDMODULE.
