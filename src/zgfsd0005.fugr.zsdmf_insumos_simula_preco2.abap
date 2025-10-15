FUNCTION zsdmf_insumos_simula_preco2.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(IV_MATNR) TYPE  MATNR
*"     REFERENCE(IV_QTDE_VENDA) TYPE  KWMENG
*"     REFERENCE(IV_UN_VENDA) TYPE  MEINS
*"     REFERENCE(IV_UN_PRECO) TYPE  MEINS
*"     REFERENCE(IV_ICBS_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(IV_ICVA_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(IV_ICMI_ATUAL) TYPE  BAPIKBETR1
*"  EXPORTING
*"     REFERENCE(EV_PR00_NOVO) TYPE  BAPIKBETR1
*"     REFERENCE(EV_RB00_NOVO) TYPE  BAPIKBETR1
*"  TABLES
*"      ET_BAPICOND STRUCTURE  BAPICOND OPTIONAL
*"----------------------------------------------------------------------

  DATA lv_menge TYPE menge_d.
  DATA lv_fator_x TYPE bapikbetr1.
  DATA lv_diferenca TYPE bapikbetr1.
  DATA lv_liq_uni TYPE bapikbetr1.
  DATA lv_liq_uni_2 TYPE bapikbetr1.
  DATA lv_dif_icmi TYPE bapikbetr1.

  IF iv_un_venda = iv_un_preco.
    lv_menge = iv_qtde_venda.
  ELSE.

    CALL FUNCTION 'MD_CONVERT_MATERIAL_UNIT'
      EXPORTING
        i_matnr              = iv_matnr
        i_in_me              = iv_un_venda
        i_out_me             = iv_un_preco
        i_menge              = CONV bstmg( iv_qtde_venda )
      IMPORTING
        e_menge              = lv_menge
      EXCEPTIONS
        error_in_application = 1
        error                = 2
        OTHERS               = 3.

    IF sy-subrc <> 0.
      lv_menge = iv_qtde_venda.
    ENDIF.

  ENDIF.

  lv_fator_x = iv_icmi_atual * ( iv_icbs_novo / 100 ) * ( iv_icva_novo / 100 ).
  lv_diferenca = iv_icmi_atual - lv_fator_x.
  lv_liq_uni = lv_diferenca / lv_menge.
  lv_liq_uni_2 = round( val = lv_liq_uni dec = 2 ).

  ev_pr00_novo = lv_liq_uni_2.
  lv_dif_icmi = lv_liq_uni_2 * lv_menge + lv_fator_x.
  ev_rb00_novo = iv_icmi_atual - lv_dif_icmi.


ENDFUNCTION.
