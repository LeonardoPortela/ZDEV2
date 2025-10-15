FUNCTION zsvpl_fcn_getworklist17_app.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(IN_USUARIO) LIKE  SY-UNAME
*"     VALUE(I_VBELN) TYPE  VBELN OPTIONAL
*"  EXPORTING
*"     VALUE(E_MSG) TYPE  CHAR50
*"     VALUE(ET_LOTES) TYPE  ZFITT_APROV_ISENCAO_JUROS_LOTE
*"     VALUE(ET_DOCS) TYPE  ZFITT_APROV_ISENCAO_JUROS_ITEM
*"  TABLES
*"      T_ESTRA STRUCTURE  ZSD_ESTRATEGIA_OV OPTIONAL
*"      T_026 STRUCTURE  ZFIT0026 OPTIONAL
*"----------------------------------------------------------------------


**----------------------------------------------------------------------*
*  DATA
**----------------------------------------------------------------------*
  DATA: ti_ordens TYPE TABLE OF zsd_ord_vendas_est_isen_juros,
        ti_estra  TYPE TABLE OF zsd_estrategia_ov,
        ti_docs   TYPE TABLE OF zsd_itens_ov_est_isen_jur.

  DATA: lv_msg   TYPE char50.

  DATA: lr_bukrs TYPE RANGE OF bukrs,
        lr_vkbur TYPE RANGE OF vkbur.

  CALL FUNCTION 'Z_OV_ESTRATEGIA_LISTA_ISEN_JUR'
    EXPORTING
      i_usuario = in_usuario
    IMPORTING
      e_msg     = lv_msg
    TABLES
      t_ordens  = ti_ordens
      t_estra   = ti_estra
      t_itens   = ti_docs.

  DELETE ti_estra WHERE opcoes <> icon_set_state.
  SORT ti_estra BY bukrs.

  "Empresa e Descempresa - Bukrs, Butxt
  SELECT companycode, companycodename, currency
   FROM i_companycodevh
   INTO TABLE @DATA(lt_empresa)
   WHERE companycode IN @lr_bukrs.

  "Esc. Venda e Desc. Venda - vbeln, vkbur
  lr_vkbur = VALUE #( FOR ls_ordem IN ti_docs
                        ( sign = 'I'
                          option = 'EQ'
                          low = ls_ordem-escr_vendas ) ).

  SELECT vkbur, bezei
    FROM tvkbt
    INTO TABLE @DATA(lt_esc)
    WHERE vkbur IN @lr_vkbur.


  LOOP AT ti_docs ASSIGNING FIELD-SYMBOL(<fs_doc>) .

    READ TABLE ti_estra WITH KEY vbeln = <fs_doc>-ov_principal TRANSPORTING NO FIELDS.
    IF sy-subrc IS NOT INITIAL.
      CONTINUE.
    ENDIF.

    READ TABLE ti_ordens ASSIGNING FIELD-SYMBOL(<fs_ordens>) WITH KEY ov_principal = <fs_doc>-ov_principal.
    IF sy-subrc = 0.

      APPEND INITIAL LINE TO et_lotes ASSIGNING FIELD-SYMBOL(<fs_lotes>).

      <fs_lotes>-simul_venda  = <fs_doc>-simul_venda.
      <fs_lotes>-ov_principal  = <fs_ordens>-ov_principal.
      <fs_lotes>-tipo_negocio  = <fs_ordens>-tipo_negocio.
      <fs_lotes>-moeda         = <fs_ordens>-moeda.
      <fs_lotes>-cliente       = <fs_ordens>-cliente.
      <fs_lotes>-desccliente   = <fs_ordens>-ds_cliente.
      <fs_lotes>-valor         = <fs_ordens>-netwr.
      <fs_lotes>-just_workflow = <fs_ordens>-just_workflow.

    ENDIF.


    APPEND INITIAL LINE TO et_docs ASSIGNING FIELD-SYMBOL(<fs_docs>).

    <fs_docs>-org_vendas     = <fs_doc>-org_vendas.
    <fs_docs>-desc_orgvendas = VALUE #( lt_empresa[ companycode = <fs_docs>-org_vendas ]-companycodename OPTIONAL ).
    <fs_docs>-escr_vendas    = <fs_doc>-escr_vendas.
    <fs_docs>-desc_escvendas = VALUE #( lt_esc[ vkbur = <fs_docs>-escr_vendas ]-bezei OPTIONAL ).
    <fs_docs>-simul_venda    = <fs_doc>-simul_venda.
    <fs_docs>-vbeln          = <fs_doc>-vbeln.
    <fs_docs>-venc_ov        = <fs_doc>-data_venc.
    <fs_docs>-moeda          = <fs_doc>-waerk.
    <fs_docs>-valor_ov       = <fs_doc>-valor_ov.
    <fs_docs>-vlr_juros_calc = <fs_doc>-vlr_juros_calc.
    <fs_docs>-vlr_juros_rbdo = <fs_doc>-vlr_juros_rbdo.
    <fs_docs>-vlr_desc_juros = <fs_doc>-vlr_desc_jros.
    <fs_docs>-saldo_juros    = <fs_doc>-saldo_juros.
    <fs_docs>-ov_principal   = <fs_ordens>-ov_principal.

  ENDLOOP.

*-----Inicio #140708 APP FIORI - Aprovação de isenção de juros ZFIS66 / PANF
  SORT et_lotes BY simul_venda ov_principal.
  DELETE ADJACENT DUPLICATES FROM et_lotes COMPARING simul_venda ov_principal.
*------Fim #140708 APP FIORI - Aprovação de isenção de juros ZFIS66 / PANF


ENDFUNCTION.
