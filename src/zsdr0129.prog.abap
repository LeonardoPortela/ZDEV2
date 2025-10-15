* ==================================================================== *
*                         © RECLIKE                                    *
* ==================================================================== *
* Program.....: ZSDR0129                                               *
* Title.......: Fretes Realizados PNL                                  *
* Author......: Ramon Barbosa de Lima                                  *
* Date........: 16/05/2022                                             *
* -------------------------------------------------------------------- *
REPORT zsdr0129.

TYPE-POOLS: slis, abap, icon.

TABLES: sscrfields, vttk.

CONSTANTS gc_internal_tab TYPE slis_tabname VALUE 'GT_DADOS_ALV'.
CONSTANTS gc_struc_name TYPE dd02l-tabname VALUE 'ZSDS066'.
CONSTANTS gc_select_field TYPE slis_fieldname VALUE 'SELEC'.
CONSTANTS gc_icon_field TYPE slis_fieldname VALUE 'ICON'.
CONSTANTS: c_post   TYPE C LENGTH 30 VALUE 'POST'.
CONSTANTS: c_delete TYPE C LENGTH 30 VALUE 'DELETE'.

TYPES:
  BEGIN OF ty_vfkp,
    fknum      TYPE fknum,
    fkpos      TYPE fkpos,
    waers      TYPE waers,
    kzwi1      TYPE vfkp-kzwi1,
    prsdt      TYPE prsdt,
    knumv      TYPE knumv,
    rebel      TYPE rebel,
    gdatu      TYPE gdatu_inv,
    kbert_zfre TYPE konv-kbetr,
    kbert_zped TYPE konv-kbetr,
  END OF ty_vfkp,

  BEGIN OF ty_vfsi,
    vbeln       TYPE vfsi-vbeln,
    posnr       TYPE vfsi-posnr,
    kzwi1       TYPE vfsi-kzwi1,
    kmein       TYPE vfsi-kmein,
    netpr       TYPE vfsi-netpr,
    knumv       TYPE vfsi-knumv,
    kposn       TYPE vfsi-kposn,
    kwert_zfre  TYPE konv-kwert,
    kwert_zped  TYPE konv-kwert,
  END OF ty_vfsi,

  BEGIN OF ty_zsdt0232,
    obj_key TYPE zsdt0232-obj_key,
    itmnum  TYPE zsdt0232-itmnum,
    refnfe  TYPE zsdt0232-refnfe,
    cuf     TYPE zsdt0232-cuf,
    aamm    TYPE zsdt0232-aamm,
    cnpj    TYPE zsdt0232-cnpj,
    mod     TYPE zsdt0232-mod,
    serie   TYPE zsdt0232-serie,
    nnf     TYPE zsdt0232-nnf,
    cpf     TYPE zsdt0232-cpf,
    ie      TYPE zsdt0232-ie,
    refcte  TYPE zsdt0232-refcte,
    docnum  TYPE j_1bdocnum,
  END OF ty_zsdt0232.



  TYPES: BEGIN OF ty_zlest0223.
           INCLUDE STRUCTURE zlest0223.
           TYPES: xqtde_env_sigam  TYPE zlest0223-qtde_total.
           TYPES: auart            TYPE vbak-auart.
           TYPES: industrializacao TYPE vbak-auart.
           TYPES: tp_trecho_v      TYPE DOMVALUE_L.
           TYPES: tp_forn_v        TYPE DOMVALUE_L,
         END OF ty_zlest0223.

  TYPES: BEGIN OF ty_dados_canc,
           idtransporte TYPE string,
           trecho       TYPE string,
           fornecimento TYPE string,
           cancel       TYPE string,
         END OF ty_dados_canc.

TYPES: BEGIN OF ty_doc_vbfa,
         docnum TYPE j_1bdocnum,
         refkey TYPE j_1brefkey,
         reftyp TYPE j_1breftyp,
         vbeln  TYPE vbeln_nach,
         vbelv  TYPE vbeln_von.
TYPES END OF ty_doc_vbfa.

TYPES: BEGIN OF ty_frete_ref,
         id_transporte     TYPE tknum,
         trecho            TYPE zde_trecho,
         fornecimento      TYPE zde_forncecimento,
         fornecimento_item TYPE lips-posnr,
         fknum             TYPE fknum.

TYPES END OF ty_frete_ref.

TYPES: BEGIN OF ty_dados,
         idtransporte               TYPE string,
         trecho                     TYPE string,
         fornecimento               TYPE string,
         fornecimentoitem           TYPE string,
         empresa                    TYPE string,
         filial                     TYPE string,
         produto                    TYPE string,
         safra                      TYPE string,
         quantidadevinc             TYPE i,     "Sigam só aceita Inteiro. Iremos avaliar a quantidade com decimais quando for implementar o cenario em questão
         valorfretereal             TYPE string,
         valorfretedolar            TYPE string,
         ValorPedagioReal           TYPE string,
         valorPedagioDolar          TYPE string,
         ValorEstadiaReal           TYPE string,
         ValorEstadiaDolar          TYPE string,
         ValorDiferencialReal       TYPE string,
         ValorDiferencialDolar      TYPE string,
         tipofornecimento           TYPE string,
         tipotransporte             TYPE string,
         portodestino               TYPE string,
         localembarque              TYPE string,
         localentrega               TYPE string,
         tipotrecho                 TYPE string,
         transbordo                 TYPE string,
         realizado                  TYPE string,
         chavereferencia            TYPE string,
         tiporomaneio               TYPE string,
         nrov                       TYPE string,
         nrpedido                   TYPE string,
         industrializacao           TYPE string,
         transferencia              TYPE string,
         intermunicipal             TYPE string,
         modal                      TYPE string,
         datadoctransporte          TYPE string,
         datadoccusto               TYPE string,
         doccusto                   TYPE string,
         doctransporte              TYPE string,
         agentefrete                TYPE string,
         proprietarioveiculo        TYPE string,
         descricaoTrecho            TYPE string,
         descricaoTipoFornecimento  TYPE string,
         descricaoModal             TYPE string,
         descricaoTipoTransporte    TYPE string,
         cancel                     TYPE string,
       END OF ty_dados.

TYPES: BEGIN OF ty_vbfa_ref,
         refkey TYPE j_1bnflin-refkey.
         INCLUDE TYPE vbfa.
       TYPES: END OF ty_vbfa_ref.

TYPES: BEGIN OF ty_cdhdr_ref,
         refkey_vt TYPE vttk-tknum,
         refkey_vl TYPE likp-vbeln.
         INCLUDE TYPE cdhdr.
       TYPES: END OF ty_cdhdr_ref.

TYPES: BEGIN OF ty_zlest0223_ref,
         ref_id_transporte     TYPE zlest0223-id_transporte,
         ref_trecho            TYPE zlest0223-trecho,
         ref_fornecimento      TYPE zlest0223-fornecimento,
         ref_fornecimento_item TYPE zlest0223-fornecimento_item,
         ref_fknum             TYPE zlest0223-fknum.
         INCLUDE TYPE zlest0223.
       TYPES: END OF ty_zlest0223_ref,

       BEGIN OF ty_ZPFE_LOTE_ITEM,
         gdatu      TYPE gdatu_inv.
         include TYPE ZPFE_LOTE_ITEM.
       TYPES: END OF ty_ZPFE_LOTE_ITEM.

 DATA:  BEGIN OF lit_group_send occurs 0,
           id_group       TYPE i,
           metodo         TYPE c LENGTH 30,
           registros_post TYPE TABLE OF ty_dados,
           registros_canc TYPE TABLE OF ty_dados_canc,
           registros_send TYPE TABLE OF ty_zlest0223,
         END OF lit_group_send.



"dados alv
DATA gt_dados_alv      TYPE STANDARD TABLE OF zsds066.
DATA gt_dados_ferro TYPE STANDARD TABLE OF zsds066. "CSB - US78840
DATA gt_dados_aqua TYPE STANDARD TABLE OF zsds066. "CSB - US78840
DATA gt_dados_ent_rodo TYPE STANDARD TABLE OF zsds066. "CSB - US78840
DATA gt_dados_ent_rodo_desmemb TYPE STANDARD TABLE OF zsds066. "CSB - US78840
DATA gt_dados_sai_rodo TYPE STANDARD TABLE OF zsds066. "CSB - US78840
DATA gt_vttp_rodo      TYPE TABLE OF vttp.
DATA gt_fieldcat       TYPE slis_t_fieldcat_alv.
DATA gt_bapiret2       TYPE TABLE OF bapiret2.
DATA gr_erdat          TYPE RANGE OF vttk-erdat.
DATA gr_aedat          TYPE RANGE OF vttk-aedat.
DATA gt_vttk           TYPE TABLE OF vttk.
DATA gt_vbfa_vt        TYPE SORTED TABLE OF vbfa WITH UNIQUE KEY vbelv posnv vbeln posnn vbtyp_n.
DATA gt_vbfa_nf        TYPE TABLE OF vbfa.
DATA gt_vbfa_ref       TYPE TABLE OF ty_vbfa_ref.
DATA gt_tvtk           TYPE TABLE OF tvtk.
DATA gt_tvtkt          TYPE TABLE OF tvtkt.
DATA gt_likp           TYPE TABLE OF likp.
DATA gt_lips           TYPE SORTED TABLE OF lips WITH UNIQUE KEY vbeln posnr.
DATA gt_vfkp           TYPE TABLE OF ty_vfkp.
DATA gt_konv           TYPE TABLE OF konv.
DATA gt_vfsi           TYPE TABLE OF ty_vfsi.
DATA gt_tvrot          TYPE TABLE OF tvrot.
DATA gt_curr_brl       TYPE TABLE OF tcurr.
DATA gt_curr_other_brl TYPE TABLE OF tcurr.
DATA gt_curr_usd       TYPE TABLE OF tcurr.
DATA gr_refkey         TYPE RANGE OF j_1brefkey.
DATA gt_lin            TYPE TABLE OF j_1bnflin.
DATA gt_doc            TYPE TABLE OF j_1bnfdoc.
DATA gt_zgr            TYPE TABLE OF zmmt_ee_zgr.
DATA gt_zgr_docs       TYPE TABLE OF zmmt_ee_zgr_docs.
DATA gt_vbfa_z1        TYPE TABLE OF vbfa.
DATA gt_vbpa_lf        TYPE TABLE OF vbpa.
DATA gt_vbak           TYPE TABLE OF vbak.
DATA gt_vbpa           TYPE TABLE OF vbpa.
DATA gt_lfa1           TYPE TABLE OF lfa1.
DATA gt_carta          TYPE TABLE OF zcarta_correcao.
"DATA gt_vtpa           TYPE TABLE OF vtpa.
DATA gt_vtpa           TYPE SORTED TABLE OF vtpa WITH UNIQUE KEY vbeln posnr parvw.
DATA gt_branch         TYPE TABLE OF j_1bbranch.
DATA gt_zcentro        TYPE TABLE OF zsdt_depara_cen.
DATA gt_ekko           TYPE TABLE OF ekko.
DATA gt_matkl_pnl    TYPE TABLE OF tvarvc.
DATA gt_maggi_algodao  TYPE TABLE OF tvarvc.
DATA gt_bukrs_pnl      TYPE TABLE OF tvarvc.
DATA gt_auart_rodo_s   TYPE TABLE OF tvarvc.
DATA gt_001_ent        TYPE TABLE OF zsdt0001.
DATA gt_001_ent_desmemb  TYPE TABLE OF zsdt0001.
DATA gt_223_ent_desmemb  TYPE TABLE OF zlest0223.
DATA gt_doc_nota_p     TYPE TABLE OF j_1bnfdoc.
DATA gt_zsdt0232       TYPE TABLE OF ty_zsdt0232.
DATA gt_001_ent_sai    TYPE TABLE OF zsdt0001.
DATA gt_xrom_saida     TYPE TABLE OF zsdt0001.
DATA gt_xrom_entrada   TYPE TABLE OF zsdt0001.
DATA gt_001_sai_ent    TYPE TABLE OF zsdt0001.
DATA gv_dias_busca TYPE i.

DATA gt_zlest0223_env  TYPE TABLE OF ty_zlest0223.

"DATA: lit_group_send   TYPE TABLE OF ty_group_send.
DATA: lwa_group_send   LIKE lit_group_send.

*** US - 78840 - Inicio - CSB
DATA: gt_vtts             TYPE TABLE OF vtts.
DATA: gt_zlest0223        TYPE TABLE OF zlest0223.
DATA: lw_zlest0223        LIKE LINE OF gt_zlest0223.
DATA: gt_zlest0223_saida  TYPE TABLE OF zlest0223.
DATA: gt_zlest0223_transb TYPE TABLE OF zlest0223.

RANGES: r_matkl_pnl    FOR lips-matkl,
        r_matkl_algodao  FOR lips-matkl,
        r_bukrs_pnl      FOR lips-matkl,
        r_auart_rs       FOR vbak-auart.


DATA: gva_xsaldodistribuir  TYPE zsds066-vfsi_xqtde_forn,
      gva_xvlrbrldistribuir TYPE zsds066-xbrl,
      gva_xvlrusddistribuir TYPE zsds066-xbrl,
      gva_xquantidadevinc   TYPE zlest0223-saldo_transb.

DATA: lv_mess TYPE string.

" 14.06.2022 - RBLIMA - DEV 80373- CS2022000013 - 7 - INTEGRAÇÃO REALIZADO FRETE -->
DATA gt_zcarta TYPE TABLE OF zcarta_correcao.
DATA gt_zpfe_lote_item TYPE TABLE OF ty_ZPFE_LOTE_ITEM.
DATA gt_doc_vbfa TYPE TABLE OF ty_doc_vbfa.
DATA gt_0223_alt_cce TYPE TABLE OF zlest0223.
DATA gt_0223_outros_val TYPE TABLE OF zlest0223.
DATA gt_cdhdr TYPE TABLE OF ty_cdhdr_ref.
DATA gt_vttk_sigam TYPE TABLE OF vttk.
DATA gt_likp_sigam TYPE TABLE OF likp.
DATA gt_0223_alt_frete TYPE TABLE OF ty_zlest0223_ref.
DATA gt_vfkk_sigam TYPE TABLE OF vfkk.
DATA gt_0223_frt_ref TYPE TABLE OF zlest0223.
" 14.06.2022 - RBLIMA - DEV 80373- CS2022000013 - 7 - INTEGRAÇÃO REALIZADO FRETE <--

DATA: zsds066 TYPE zsds066.

*** US - 78840 - Fim - CSB

"SELECTION-SCREEN: FUNCTION KEY 1

" DADOS PRINCIPAIS
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS so_tknum FOR vttk-tknum.
SELECT-OPTIONS so_erdat FOR vttk-erdat.
SELECT-OPTIONS so_aedat FOR vttk-aedat.
SELECTION-SCREEN END OF BLOCK b1.

" DADOS SECUNDARIOS
SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.
PARAMETERS p_dias TYPE int4.
PARAMETERS p_vari TYPE slis_vari NO-DISPLAY.
PARAMETERS p_visu AS CHECKBOX.

*** CSB - Inicio
PARAMETERS p_erodo AS CHECKBOX. "Frete Entrada Rodoviario
PARAMETERS p_srodo AS CHECKBOX. "Frete Saida Rodoviario
PARAMETERS p_sfer AS CHECKBOX.  "Frete Saida Ferroviario
PARAMETERS p_saqu AS CHECKBOX.  "Frete Saida Aquaviario
PARAMETERS p_alte AS CHECKBOX.  "Processamento Alterações Frete
PARAMETERS p_sigm AS CHECKBOX.  "Envio realizado ao SIGAM
*** CSB - Fim

SELECTION-SCREEN END OF BLOCK b2.

INITIALIZATION.
  "PERFORM f_preenche_data.
  "PERFORM F_BOTAO_FUNCTION.
  PERFORM default_variant CHANGING p_vari.

*AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari.
*  PERFORM f4_for_variant CHANGING p_vari.

AT SELECTION-SCREEN.
  PERFORM f_botao_command.

START-OF-SELECTION.

  TRY.
      zcl_job=>get_ck_program_execucao( EXPORTING i_nome_program = sy-cprog IMPORTING e_qtd  = DATA(e_qtd) ).
    CATCH zcx_job.
  ENDTRY.

  IF e_qtd GT 1.
    LEAVE PROGRAM.
  ENDIF.

  PERFORM f_preenche_data.
  PERFORM f_get_stvarvs.

*-----------------------------------------------------------*
* Seleção Geral Dados Transporte - US 80244
*-----------------------------------------------------------*
  PERFORM f_selec_inicial.
  PERFORM f_proce_inicial.


*-----------------------------------------------------------*
*  14.06.2022 - RBLIMA - DEV 80373- CS2022000013 - 7 - INTEGRAÇÃO REALIZADO FRETE -->
*-----------------------------------------------------------*

  IF p_alte = 'X'.
    PERFORM f_selec_alteracao_canc_transp.
    PERFORM f_proce_alteracao_canc_transp.

    PERFORM f_selec_outros_val_transp.
    PERFORM f_proce_outros_val_transp.
  ENDIF.

*-----------------------------------------------------------*
*   Frete Entrada Rodoviario - US 76638
*-----------------------------------------------------------*
  IF p_erodo = 'X'.
    PERFORM f_selec_dados_trecho_ent.
    PERFORM f_proce_dados_trecho_ent.

    PERFORM f_selec_dados_rodo_ent.
    PERFORM f_proce_dados_rodo_ent.

    IF ( sy-batch IS NOT INITIAL ) OR ( p_visu IS INITIAL ).
      PERFORM f_grava_dados_rodo_ent.
    ENDIF.
  ENDIF.

*-----------------------------------------------------------*
*   Frete Saida Rodoviario - US 78597
*-----------------------------------------------------------*
  IF p_srodo = 'X'.
    IF ( sy-batch IS NOT INITIAL ) OR ( p_visu IS INITIAL ).
      PERFORM f_selec_dados_rodo_sai.
      PERFORM f_grava_dados_rodo_sai.
    ENDIF.
  ENDIF.
*-----------------------------------------------------------*
*   Frete Saida Ferroviario  - US 78840
*-----------------------------------------------------------*
*** US - 78840 - Inicio - CBRAND
  IF p_sfer = 'X'.
    IF ( sy-batch IS NOT INITIAL ) OR ( p_visu IS INITIAL ).
      PERFORM f_selec_dados_ferro_sai.
      PERFORM f_grava_dados_ferro_sai.
    ENDIF.
  ENDIF.
*** US - 78840 - Fim - CBRAND


  IF p_saqu = 'X'.
    IF ( sy-batch IS NOT INITIAL ) OR ( p_visu IS INITIAL ).
      PERFORM f_selec_dados_aqua_sai.
      PERFORM f_grava_dados_aqua_sai.
    ENDIF.
  ENDIF.


**20.06.2022 - US - 78929 - CS2022000013 -8 - Integração Realizado Frete- Envio ao Sigam - CBRAND - Inicio
  IF p_sigm = 'X'.
    PERFORM f_envio_sigam_frete_realizado.
  ENDIF.

  IF ( sy-batch IS INITIAL ) AND ( p_visu IS NOT INITIAL ).
    PERFORM f_exibe_alv.
  ENDIF.

  CHECK 1 = 2.
  PERFORM f_delete_base. "Limpar Base para Carga


**20.06.2022 - US - 78929 - CS2022000013 -8 - Integração Realizado Frete- Envio ao Sigam - CBRAND - Fim
*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA
*&---------------------------------------------------------------------*
FORM f_selec_inicial .

  DATA lv_data TYPE char08.

  CLEAR gr_refkey.

  " 2.3.1  Seleção inicial
  CHECK ( gr_erdat[] IS NOT INITIAL ) OR ( gr_aedat[] IS NOT INITIAL ) OR ( so_tknum[] IS NOT INITIAL ).

  SELECT * FROM vttk
    INTO TABLE gt_vttk
      WHERE aedat IN gr_aedat
        AND erdat IN gr_erdat
        AND tknum IN so_tknum
        AND abfer IN ( '1', '2' ). "1 = saida  / 2 = Entrada

  CHECK gt_vttk[] IS NOT INITIAL.

*** US - 78840 - Inicio - CBRAND
  SELECT * FROM vtts
  INTO TABLE gt_vtts
    FOR ALL ENTRIES IN gt_vttk
      WHERE tknum EQ gt_vttk-tknum.
*** US - 78840 - Fim - CBRAND

  " 2.3.2 Buscar a classificação do tipo de transporte
  SELECT * FROM tvtk
    INTO TABLE gt_tvtk
      FOR ALL ENTRIES IN gt_vttk
        WHERE shtyp =  gt_vttk-shtyp.

  SELECT * FROM tvtkt
    INTO TABLE gt_tvtkt
      FOR ALL ENTRIES IN gt_vttk
        WHERE shtyp =  gt_vttk-shtyp
          AND spras = sy-langu.

  " 2.3.3 Buscar os documentos de faturamento (remessas e avisos) vinculados as VTs (doc de transporte)
  SELECT * FROM vbfa
    INTO TABLE gt_vbfa_vt
      FOR ALL ENTRIES IN gt_vttk
        WHERE vbeln =   gt_vttk-tknum
          AND vbtyp_n  = '8'
          AND vbtyp_v  IN ( 'J', '7' ).

  CHECK sy-subrc EQ 0.

  " 2.3.4 Buscar dados da LIKP
  SELECT * FROM likp
    INTO TABLE gt_likp
      FOR ALL ENTRIES IN gt_vbfa_vt
        WHERE vbeln =  gt_vbfa_vt-vbelv.

  " 2.3.5 Buscar dados da LIPS
  SELECT * FROM lips
    INTO TABLE gt_lips
        FOR ALL ENTRIES IN gt_vbfa_vt
          WHERE vbeln =  gt_vbfa_vt-vbelv
             AND matkl IN r_matkl_pnl. "CSB 09.06.2022


  IF  gt_lips[] IS NOT INITIAL.
    "Verificar se a informação no campo VBELN é nr de OV ou de Pedido
    SELECT * FROM vbak
      INTO TABLE gt_vbak
      FOR ALL ENTRIES IN gt_lips
        WHERE vbeln = gt_lips-vgbel.

    SELECT * FROM ekko
      INTO TABLE gt_ekko
      FOR ALL ENTRIES IN gt_lips
        WHERE ebeln = gt_lips-vgbel.
  ENDIF.


  " 2.3.6 Buscar os valores de frete
  SELECT fknum fkpos waers kzwi1 prsdt knumv rebel FROM vfkp
    INTO CORRESPONDING FIELDS OF TABLE gt_vfkp
    FOR ALL ENTRIES IN gt_vttk
      WHERE rebel = gt_vttk-tknum
        AND fkpty = 'Z001'.

  IF gt_vfkp[] IS NOT INITIAL.
    SELECT FROM V_KONV FIELDS * FOR ALL ENTRIES IN @GT_VFKP WHERE KNUMV EQ @GT_VFKP-KNUMV INTO CORRESPONDING FIELDS OF TABLE @GT_KONV .

    DELETE gt_konv WHERE kinak NE SPACE.
  ENDIF.

  delete gt_vfkp WHERE prsdt is INITIAL.
  delete gt_vfkp WHERE kzwi1 is INITIAL.

  LOOP AT gt_vfkp ASSIGNING FIELD-SYMBOL(<fs_vfkp>).

    lv_data = <fs_vfkp>-prsdt+6(2) && <fs_vfkp>-prsdt+4(2) && <fs_vfkp>-prsdt(4).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_data
      IMPORTING
        output = <fs_vfkp>-gdatu.

    READ TABLE gt_konv INTO DATA(lwa_konv) WITH KEY knumv = <fs_vfkp>-knumv
                                                    kschl = 'ZFRE'.

    IF sy-subrc EQ 0.
      if lwa_konv-waers NE 'BRL'.
        <fs_vfkp>-kbert_zfre = lwa_konv-kbetr * lwa_konv-kkurs.
      ELSE.
        <fs_vfkp>-kbert_zfre = lwa_konv-kbetr.
      ENDIF.
    ENDIF.

    READ TABLE gt_konv INTO lwa_konv WITH KEY knumv = <fs_vfkp>-knumv
                                              kschl = 'ZPED'.

    IF sy-subrc EQ 0.
      if lwa_konv-waers NE 'BRL'.
        <fs_vfkp>-kbert_zped = lwa_konv-kbetr * lwa_konv-kkurs.
      ELSE.
        <fs_vfkp>-kbert_zped = lwa_konv-kbetr.
      ENDIF.
    ENDIF.

  ENDLOOP.

  SELECT * FROM vfsi
    INTO CORRESPONDING FIELDS OF TABLE gt_vfsi
      FOR ALL ENTRIES IN gt_vfkp
        WHERE knumv = gt_vfkp-knumv.

  LOOP AT gt_vfsi ASSIGNING FIELD-SYMBOL(<fs_vfsi>).

    READ TABLE gt_vfkp INTO DATA(lwa_vfkp) WITH KEY knumv = <fs_vfsi>-knumv.

    CHECK sy-subrc eq 0.

    if lwa_vfkp-waers NE 'BRL'.
      MESSAGE 'Cenário não previsto! Deve ser analisado.. Cancelando JOB....' TYPE 'E'.
    ENDIF.

    READ TABLE gt_konv INTO lwa_konv WITH KEY knumv = <fs_vfsi>-knumv
                                              kposn = <fs_vfsi>-kposn
                                              kschl = 'ZFRE'.

    IF sy-subrc EQ 0.
      if lwa_vfkp-waers NE 'BRL'.
        <fs_vfsi>-kwert_zfre = lwa_konv-kwert * lwa_konv-kkurs.
      ELSE.
        <fs_vfsi>-kwert_zfre = lwa_konv-kwert.
      ENDIF.
    ENDIF.

    READ TABLE gt_konv INTO lwa_konv WITH KEY knumv = <fs_vfsi>-knumv
                                              kposn = <fs_vfsi>-kposn
                                              kschl = 'ZPED'.

    IF sy-subrc EQ 0.
      if lwa_vfkp-waers NE 'BRL'.
        <fs_vfsi>-kwert_zped = lwa_konv-kwert * lwa_konv-kkurs.
      ELSE.
        <fs_vfsi>-kwert_zped = lwa_konv-kwert.
      ENDIF.
    ENDIF.

  ENDLOOP.


  IF gt_vfkp IS NOT INITIAL.

    SELECT * FROM tcurr
      INTO TABLE gt_curr_brl
      FOR ALL ENTRIES IN gt_vfkp
        WHERE kurst = 'B'
          AND fcurr = 'BRL'
          AND tcurr = 'USD'
          AND gdatu = gt_vfkp-gdatu.

    SELECT * FROM tcurr
      INTO TABLE gt_curr_usd
      FOR ALL ENTRIES IN gt_vfkp
        WHERE kurst = 'B'
          AND fcurr = 'USD'
          AND tcurr = 'BRL'
          AND gdatu = gt_vfkp-gdatu.

  ENDIF.

  " 2.3.7  Buscar descritivo do Itinerário
  SELECT * FROM tvrot
    INTO TABLE gt_tvrot
    FOR ALL ENTRIES IN gt_vttk
      WHERE route = gt_vttk-route
        AND spras = sy-langu.

  " 2.3.8  Buscar Docnum e NF vinculados ao transporte

  SELECT * FROM vbfa
    INTO TABLE gt_vbfa_nf
    FOR ALL ENTRIES IN gt_vbfa_vt
      WHERE vbelv   EQ gt_vbfa_vt-vbelv
        AND vbtyp_n IN ( 'M' , 'R' ).

  LOOP AT gt_vbfa_nf ASSIGNING FIELD-SYMBOL(<fs_vbfa>).

    APPEND INITIAL LINE TO gt_vbfa_ref ASSIGNING FIELD-SYMBOL(<fs_vbfa_ref>).

    IF <fs_vbfa>-vbtyp_n = 'M'.
      <fs_vbfa_ref>-refkey = <fs_vbfa>-vbeln.
    ENDIF.

    IF <fs_vbfa>-vbtyp_n  = 'R'.
      <fs_vbfa_ref>-refkey = <fs_vbfa>-vbeln && <fs_vbfa>-erdat(4).
    ENDIF.

*    IF <fs_vbfa>-vbtyp_n = 'M'.
*      APPEND 'IEQ' && <fs_vbfa>-vbeln TO gr_refkey.
*    ENDIF.
*
*    IF <fs_vbfa>-vbtyp_n  = 'R'.
*      APPEND 'IEQ' && <fs_vbfa>-vbeln && <fs_vbfa>-erdat(4) TO gr_refkey.
*    ENDIF.

  ENDLOOP.

  IF gt_vbfa_ref[] IS NOT INITIAL.

    SELECT * FROM j_1bnflin
      INTO TABLE gt_lin
      FOR ALL ENTRIES IN gt_vbfa_ref
        WHERE refkey EQ gt_vbfa_ref-refkey.

    IF gt_lin[] IS NOT INITIAL.

      SELECT * FROM j_1bnfdoc
        INTO TABLE gt_doc
          FOR ALL ENTRIES IN gt_lin
            WHERE docnum = gt_lin-docnum.

    ENDIF.

  ENDIF.

  IF gt_vbfa_vt[] IS NOT INITIAL.

    SELECT * FROM zmmt_ee_zgr_docs
      INTO TABLE gt_zgr_docs
        FOR ALL ENTRIES IN gt_vbfa_vt
          WHERE av_vbeln = gt_vbfa_vt-vbelv.

    IF gt_zgr_docs[] IS NOT INITIAL.

      SELECT * FROM j_1bnfdoc
    APPENDING TABLE gt_doc
      FOR ALL ENTRIES IN gt_zgr_docs
        WHERE docnum = gt_zgr_docs-docnum.

      LOOP AT gt_doc INTO DATA(lwa_doc) WHERE candat IS NOT INITIAL.
        DELETE gt_zgr_docs WHERE docnum = lwa_doc-docnum.
      ENDLOOP.

    ENDIF.

  ENDIF.

  " 2.3.9 - Buscar parceiro Z1 das OVs ZRFL, ZDCO ou ZIND
  SELECT * FROM vbfa
    INTO TABLE gt_vbfa_z1
      FOR ALL ENTRIES IN gt_vbfa_vt
        WHERE vbeln = gt_vbfa_vt-vbelv
          AND vbtyp_n  = 'J'
          AND vbtyp_v  = 'C'.

  IF gt_vbak[] IS NOT INITIAL.

    SELECT * FROM vbpa
        INTO TABLE gt_vbpa
        FOR ALL ENTRIES IN gt_vbak
          WHERE vbeln = gt_vbak-vbeln
            AND parvw = 'Z1'.

    IF gt_vbpa[] IS NOT INITIAL.
      SELECT * FROM lfa1
        INTO TABLE gt_lfa1
          FOR ALL ENTRIES IN gt_vbpa
            WHERE lifnr = gt_vbpa-lifnr
              AND land1 = 'BR'.
    ENDIF.

  ENDIF.

  IF gt_doc IS NOT INITIAL.

    " 2.3.10 - Verificar se tem carta de correção para corrigir o parceiro Z1
    SELECT * FROM zcarta_correcao
      INTO TABLE gt_carta
        FOR ALL ENTRIES IN gt_doc
          WHERE docnum = gt_doc-docnum
            AND authcode <> ''
            AND novo_terminal <> ''.

    SORT gt_carta BY dt_authcod hr_authcod DESCENDING.

  ENDIF.

  " 2.3.11 - selecionar o parceiro LF do aviso de recebimento/remessa
  IF gt_vbfa_vt IS NOT INITIAL.

    SELECT * FROM vbpa
      INTO TABLE gt_vbpa_lf
      FOR ALL ENTRIES IN gt_vbfa_vt
        WHERE vbeln = gt_vbfa_vt-vbelv
          AND parvw = 'LF'.
  ENDIF.

  IF gt_vttk IS NOT INITIAL.

    " 2.3.12 - Selecione os parceiros do transporte
    SELECT * FROM vtpa
      INTO TABLE gt_vtpa
        FOR ALL ENTRIES IN gt_vttk
          WHERE vbeln = gt_vttk-tknum
            AND parvw IN ('PC','LR','PV','SP').

    " 2.3.13  Busca empresa/filial a partir da Organização de transporte
    SELECT * FROM j_1bbranch
      INTO TABLE gt_branch
            FOR ALL ENTRIES IN gt_vttk
            WHERE branch = gt_vttk-tplst.

    SELECT * FROM zsdt_depara_cen
      INTO TABLE gt_zcentro
        FOR ALL ENTRIES IN gt_vttk
                  WHERE centrov_1 = gt_vttk-tplst.

  ENDIF.
ENDFORM.                    " F_SELECIONA
*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM f_user_command  USING r_ucomm     TYPE sy-ucomm
            rs_selfield TYPE slis_selfield.                 "#EC CALLED

  CASE r_ucomm.
    WHEN '&IC1'.
      PERFORM f_hyperlink   USING rs_selfield.
    WHEN 'EXIT'.
      LEAVE TO SCREEN 0.
    WHEN OTHERS.
  ENDCASE.

  rs_selfield-refresh = 'X'.
  r_ucomm = '&REFRESH'.

ENDFORM.                    "user_command
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_FUNCTION
*&---------------------------------------------------------------------*
FORM f_botao_function.

  sscrfields-functxt_01 = 'BOTAO 1'.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_BOTAO_COMMAND
*&---------------------------------------------------------------------*
FORM f_botao_command.

  IF sy-ucomm = 'FC01'.
    "EXECUTA FUNÇÃO DO BOTAO 1
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_EXIBE_ALV
*&---------------------------------------------------------------------*
FORM f_exibe_alv .

  DATA lw_layout TYPE slis_layout_alv.
  DATA lw_variant TYPE disvariant.

  IF gt_dados_alv IS NOT INITIAL.

    IF p_vari IS NOT INITIAL.
      lw_variant-report = sy-repid.
      lw_variant-variant = p_vari.
    ENDIF.

    PERFORM f_monta_fieldcat.

    lw_layout-zebra             = abap_true.
    lw_layout-colwidth_optimize = abap_true.
    lw_layout-box_fieldname = gc_select_field.
    lw_layout-info_fieldname = 'COLOR'.

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'F_STATUS_SET'
        i_callback_user_command  = 'F_USER_COMMAND'
        is_layout                = lw_layout
        it_fieldcat              = gt_fieldcat
        i_save                   = 'A'
        is_variant               = lw_variant
      TABLES
        t_outtab                 = gt_dados_alv
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.

    IF sy-subrc <> 0.
      PERFORM f_mensagem_sistema.
    ENDIF.

  ELSE.
    MESSAGE s213(v4) DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

ENDFORM.                    " F_EXIBE_ALV
*&---------------------------------------------------------------------*
*&      Form  F_PFSTATUS
*&---------------------------------------------------------------------*
FORM f_status_set USING p_extab TYPE slis_t_extab.          "#EC CALLED

  SET PF-STATUS 'STANDARD'.

ENDFORM.                    "F_PFSTATUS
*&---------------------------------------------------------------------*
*&      Form  F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
FORM f_monta_fieldcat.

  "LVC_FIELDCATALOG_MERGE
  " SET PARAMETER ID 'ALVBUFFER' FIELD sy-datum.
  CALL FUNCTION 'REUSE_ALV_FIELDCATALOG_MERGE'
    EXPORTING
      i_program_name         = sy-cprog
      i_internal_tabname     = gc_internal_tab
      i_structure_name       = gc_struc_name
    CHANGING
      ct_fieldcat            = gt_fieldcat
    EXCEPTIONS
      inconsistent_interface = 1
      program_error          = 2
      OTHERS                 = 3.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_fcat>)
    WITH KEY fieldname = gc_icon_field.

  IF sy-subrc EQ 0.
    <fs_fcat>-just = 'C'.
    <fs_fcat>-reptext_ddic = 'Status'.
    <fs_fcat>-ddic_outputlen = 000010.
  ENDIF.

  DELETE gt_fieldcat WHERE fieldname = gc_select_field.
  DELETE gt_fieldcat WHERE fieldname = 'COLOR'.

  PERFORM f_coluna_edita USING 'MSGTX' 'Mensagem'.

  PERFORM f_coluna_edita USING 'LIFNR_LF'          'Parc.LF'.
  PERFORM f_coluna_edita USING 'LFA1_NAME1'        'LF Descr.'.
  PERFORM f_coluna_edita USING 'NOVO_TERMINAL'     'Terminal'.
  PERFORM f_coluna_edita USING 'LIFNR_PC'          'Parc.PC'.
  PERFORM f_coluna_edita USING 'LIFNR_PV'          'Parc.PV'.
  PERFORM f_coluna_edita USING 'LIFNR_SP'          'Parc.SP'.
  PERFORM f_coluna_edita USING 'KUNNR_LR'          'Parc.LR'.
  PERFORM f_coluna_edita USING 'ROMA_ENT_CH_REF'   'Chave.RomaEntr'.
  PERFORM f_coluna_edita USING 'ROMA_ENT_BRANCH'   'Local.RomaEntr'.
  PERFORM f_coluna_edita USING 'ROMA_ENT_SAFRA'    'Safra.RomaEntr'.
  PERFORM f_coluna_edita USING 'ROMA_ENT_BUKRS'    'Empre.RomaEntr'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_CH_REF'   'Chave.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_BRANCH'   'Local.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_SAFRA'    'Safra.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_BUKRS'    'Empre.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_DOC_REM'  'Doc.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_TKNUM'    'Trans.RomaSai'.
  PERFORM f_coluna_edita USING 'ROMA_SAI_FKNUM'    'Frete.RomaSai'.

  PERFORM f_coluna_edita USING 'XBRL' 'Vlr.Reais'.
  PERFORM f_coluna_edita USING 'XUSD' 'Vlr.Dolar'.

  "PERFORM f_coluna_edita USING 'COUNT_SAP' 'Notas SAP'.


ENDFORM.                    " F_MONTA_FIELDCAT
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE sy-msgty.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_s.

  MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 DISPLAY LIKE 'E'.

ENDFORM.                    " F_MENSAGEM_SISTEMA
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_SISTEMA_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_sistema_insere.

  PERFORM f_mensagem_insere
     USING sy-msgty
           sy-msgid
           sy-msgno
           sy-msgv1
           sy-msgv2
           sy-msgv3
           sy-msgv4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SAP_INDICATOR
*&---------------------------------------------------------------------*
FORM f_sap_indicator USING p_text TYPE c
                           p_percent TYPE i.

  CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
    EXPORTING
      percentage = p_percent
      text       = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F4_FOR_VARIANT
*&---------------------------------------------------------------------*
FORM f4_for_variant CHANGING f_vari TYPE slis_vari.

  DATA: lw_variant TYPE disvariant.

  lw_variant-variant = f_vari.
  lw_variant-report = sy-repid.

  CALL FUNCTION 'REUSE_ALV_VARIANT_F4'
    EXPORTING
      is_variant = lw_variant
      i_save     = 'A'
    IMPORTING
      es_variant = lw_variant
    EXCEPTIONS
      not_found  = 2.

  IF sy-subrc = 2.
    MESSAGE ID sy-msgid TYPE 'S' NUMBER sy-msgno
    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ELSE.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    "f4_for_variant
*&---------------------------------------------------------------------*
*&      Form  DEFAULT_VARIANT
*&---------------------------------------------------------------------*
FORM default_variant CHANGING f_vari TYPE slis_vari.
  DATA: lw_variant TYPE disvariant.

  lw_variant-report = sy-repid.
  CALL FUNCTION 'REUSE_ALV_VARIANT_DEFAULT_GET'
    EXPORTING
      i_save        = 'A'
    CHANGING
      cs_variant    = lw_variant
    EXCEPTIONS
      wrong_input   = 1
      not_found     = 2
      program_error = 3
      OTHERS        = 4.

  IF sy-subrc = 0.
    f_vari = lw_variant-variant.
  ENDIF.

ENDFORM.                    " DEFAULT_VARIANT
*&---------------------------------------------------------------------*
*&      Form  F_PREENCHE_DATA
*&---------------------------------------------------------------------*
FORM f_preenche_data .

  CLEAR: gr_erdat, gr_aedat.

  gr_erdat[] = so_erdat[].

  IF p_dias IS NOT INITIAL.

    APPEND INITIAL LINE TO gr_aedat ASSIGNING FIELD-SYMBOL(<fs_aedat>).

    <fs_aedat>-sign = 'I'.
    <fs_aedat>-option = 'BT'.
    <fs_aedat>-low = sy-datum.
    SUBTRACT p_dias FROM <fs_aedat>-low.
    <fs_aedat>-high = sy-datum.

  ELSE.

    gr_aedat[] = so_aedat[].

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_HYPERLINK
*&---------------------------------------------------------------------*
FORM f_hyperlink USING rs_selfield TYPE slis_selfield.

  DATA lw_saida_alv LIKE LINE OF gt_dados_alv.

  CHECK rs_selfield-value IS NOT INITIAL.

  READ TABLE gt_dados_alv INTO lw_saida_alv INDEX rs_selfield-tabindex.

  CASE rs_selfield-fieldname.
    WHEN 'COLUNA'.
    WHEN OTHERS.
  ENDCASE.

ENDFORM.                    " F_HYPERLINK

"PERFORM f_mensagem_insere TABLES p_ret2
"USING 'E' 'ZMM' '000' 'SYSID' text-t02
"gw_034-logsys space space.

FORM f_mensagem_bapiret USING p_mess TYPE bapiret2.

  MESSAGE ID p_mess-id TYPE 'S' NUMBER p_mess-number
    WITH p_mess-message_v1 p_mess-message_v2
         p_mess-message_v3 p_mess-message_v4.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_MENSAGEM_INSERE
*&---------------------------------------------------------------------*
FORM f_mensagem_insere USING i_type TYPE bapi_mtype
                              i_id  TYPE  symsgid
                              i_number  TYPE  symsgno
                              i_mess_v1 TYPE any
                              i_mess_v2 TYPE any
                              i_mess_v3 TYPE any
                              i_mess_v4 TYPE any.

  APPEND INITIAL LINE TO gt_bapiret2 ASSIGNING FIELD-SYMBOL(<fs_ret>).

  <fs_ret>-type = i_type.
  <fs_ret>-id = i_id.
  <fs_ret>-number = i_number.
  <fs_ret>-message_v1 = i_mess_v1.
  <fs_ret>-message_v2 = i_mess_v2.
  <fs_ret>-message_v3 = i_mess_v3.
  <fs_ret>-message_v4 = i_mess_v4.
  <fs_ret>-system = sy-sysid.

  IF sy-batch IS INITIAL.

    MESSAGE ID <fs_ret>-id TYPE <fs_ret>-type NUMBER <fs_ret>-number
      WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
        <fs_ret>-message_v4 INTO <fs_ret>-message.

  ELSE.

    MESSAGE ID <fs_ret>-id TYPE 'S' NUMBER <fs_ret>-number
          WITH <fs_ret>-message_v1 <fs_ret>-message_v2 <fs_ret>-message_v3
            <fs_ret>-message_v4 DISPLAY LIKE <fs_ret>-type.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_EXIBE_POPUP
*&---------------------------------------------------------------------*
FORM f_mensagem_exibe_popup USING p_bapiret2_tab TYPE bapiret2_t.

  DATA: l_lines TYPE i.

  DESCRIBE TABLE p_bapiret2_tab LINES l_lines.

  IF l_lines <= 1 OR sy-batch = 'X'.

    LOOP AT p_bapiret2_tab ASSIGNING FIELD-SYMBOL(<fs_ret2>).

      MESSAGE ID <fs_ret2>-id
            TYPE 'S'
          NUMBER <fs_ret2>-number
            WITH <fs_ret2>-message_v1
                 <fs_ret2>-message_v2
                 <fs_ret2>-message_v3
                 <fs_ret2>-message_v4 DISPLAY LIKE <fs_ret2>-type.

    ENDLOOP.

  ELSE.

    CALL FUNCTION 'MESSAGES_INITIALIZE'.

    LOOP AT p_bapiret2_tab ASSIGNING <fs_ret2>.

      IF <fs_ret2>-id IS INITIAL.

        <fs_ret2>-id = 'DS'. "<-classe padrao abap
        <fs_ret2>-number = '016'.
        <fs_ret2>-message_v1 = <fs_ret2>-message.

      ENDIF.

      CALL FUNCTION 'MESSAGE_STORE'
        EXPORTING
          arbgb                  = <fs_ret2>-id
          "EXCEPTION_IF_NOT_ACTIVE  = 'X'
          msgty                  = <fs_ret2>-type
          msgv1                  = <fs_ret2>-message_v1
          msgv2                  = <fs_ret2>-message_v2
          msgv3                  = <fs_ret2>-message_v3
          msgv4                  = <fs_ret2>-message_v4
          txtnr                  = <fs_ret2>-number
          "ZEILE                    = ' '
          "IMPORTING
          "ACT_SEVERITY             =
          "MAX_SEVERITY             =
        EXCEPTIONS
          message_type_not_valid = 1
          not_active             = 2
          OTHERS                 = 3.     "#EC CI_SUBRC

    ENDLOOP.

    CALL FUNCTION 'MESSAGES_STOP'
      EXCEPTIONS
        a_message = 1
        e_message = 2
        i_message = 3
        w_message = 4
        OTHERS    = 5.     "#EC CI_SUBRC

    CALL FUNCTION 'MESSAGES_SHOW'
      EXPORTING
        "CORRECTIONS_OPTION          = ' '
        "CORRECTIONS_FUNC_TEXT       = ' '
        "LINE_FROM                   = ' '
        "LINE_TO                     = ' '
        "OBJECT                      = ' '
        "SEND_IF_ONE                 = ' '
        batch_list_type     = 'B'
        show_linno          = ' '
        show_linno_text     = 'X'
        show_linno_text_len = '3'
        i_use_grid          = ' '
        i_amodal_window     = ' '
        "MSG_SELECT_FUNC             = ' '
        "MSG_SELECT_FUNC_TEXT        = ' '
        "IMPORTING
        "CORRECTIONS_WANTED          =
        "E_EXIT_COMMAND              =
        "MSG_SELECTED                =
      EXCEPTIONS
        inconsistent_range  = 1
        no_messages         = 2
        OTHERS              = 3.     "#EC CI_SUBRC

  ENDIF.

ENDFORM.
* CONTROLE DE ATUALIZAÇÃO DE TELA DINAMICAMENTE

*    DATA lt_return TYPE TABLE OF ddshretval.
*    DATA lt_fields TYPE TABLE OF dynpread.
*
*    CALL FUNCTION 'F4IF_FIELD_VALUE_REQUEST'
*      EXPORTING
*        tabname           = 'ZTPP_009'
*        fieldname         = 'MATNR_DUMMY'
*        "searchhelp        = 'ZHPP_DUMMY'
*        "shlpparam         = 'MATNR_DUMMY'
*        "IMPORTING
*        "user_reset        =
*      TABLES
*        return_tab        = lt_return
*      EXCEPTIONS
*        field_not_found   = 1
*        no_help_for_field = 2
*        inconsistent_help = 3
*        no_values_found   = 4
*        OTHERS            = 5.
*
*    IF sy-subrc <> 0.
*      EXIT.
*    ENDIF.
*
*    LOOP AT lt_return ASSIGNING FIELD-SYMBOL(<fs_ret>).
*
*      APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn>).
*
**      stepl
**
**      fieldinp
*
*      CASE <fs_ret>-fieldname.
*        WHEN 'WERKS'.
*          <fs_dyn>-fieldname = 'P_WERKS'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'ARBPL'.
*          <fs_dyn>-fieldname = 'P_ARBPL'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*        WHEN 'VORNR'.
*
*          <fs_dyn>-fieldname = 'P_VORNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*        WHEN 'FLAG_DUMPS'.
*
*          APPEND INITIAL LINE TO lt_fields ASSIGNING FIELD-SYMBOL(<fs_dyn2>).
*
*          IF <fs_ret>-fieldval = 'X'.
*
*            <fs_dyn>-fieldname = 'P_DUM_S'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_N'.
*            <fs_dyn2>-fieldvalue = space.
*
*          ELSE.
*            <fs_dyn>-fieldname = 'P_DUM_N'.
*            <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*            <fs_dyn2>-fieldname = 'P_DUM_S'.
*            <fs_dyn2>-fieldvalue = space.
*          ENDIF.
*
*        WHEN 'MATNR_DUMMY'.
*
*          <fs_dyn>-fieldname = 'P_MATNR'.
*          <fs_dyn>-fieldvalue = <fs_ret>-fieldval.
*
*
*      ENDCASE.
*
*    ENDLOOP.
*
*    CALL FUNCTION 'DYNP_VALUES_UPDATE'
*      EXPORTING
*        dyname               = '1000'
*        dynumb               = '1000'
*      TABLES
*        dynpfields           = lt_fields
*      EXCEPTIONS
*        invalid_abapworkarea = 1
*        invalid_dynprofield  = 2
*        invalid_dynproname   = 3
*        invalid_dynpronummer = 4
*        invalid_request      = 5
*        no_fielddescription  = 6
*        undefind_error       = 7
*        OTHERS               = 8.
*
*    IF sy-subrc <> 0.
** Implement suitable error handling here
*    ENDIF.


***AT SELECTION-SCREEN OUTPUT.
**
**    LOOP AT SCREEN.
**
**      IF screen-name CP '*P_WERKS*'
**        OR screen-name CP '*P_DUM_N*'
**        OR screen-name CP '*P_ARBPL*'
**        OR screen-name CP '*P_VORNR*'.
**
**        screen-input = 0.
**        MODIFY SCREEN.
**      ENDIF.
**
**    ENDLOOP.
*&---------------------------------------------------------------------*
*&      Form  F_COLUNA_EDITA
*&---------------------------------------------------------------------*
FORM f_coluna_edita  USING p_fieldname TYPE slis_fieldname
                           p_text TYPE scrtext_l.

  READ TABLE gt_fieldcat ASSIGNING FIELD-SYMBOL(<fs_cat>)
    WITH KEY fieldname = p_fieldname.

  CHECK sy-subrc EQ 0.

  <fs_cat>-seltext_s = p_text.
  <fs_cat>-seltext_m = p_text.
  <fs_cat>-seltext_l = p_text.
  <fs_cat>-reptext_ddic = p_text.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form F_VERIFICA_LINHA_SELEC
*&---------------------------------------------------------------------*
FORM f_verifica_linha_selec CHANGING p_error TYPE c.

  READ TABLE gt_dados_alv WITH KEY selec = 'X' TRANSPORTING NO FIELDS.

  IF sy-subrc NE 0.
    MESSAGE s851(v4) DISPLAY LIKE 'E'.
    p_error = 'X'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_processa
*&---------------------------------------------------------------------*
FORM f_processa.

  DATA lr_selec TYPE RANGE OF flag.

  DATA lv_ret.

  IF sy-batch IS INITIAL.

    PERFORM f_verifica_linha_selec CHANGING lv_ret.

    CHECK lv_ret IS INITIAL.

    PERFORM f_popup_to_confirm USING text-t01 CHANGING lv_ret.

    CHECK lv_ret = '1'.

    APPEND 'IEQX' TO lr_selec.

  ELSE.
    CLEAR lr_selec.

  ENDIF.

  LOOP AT gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE selec IN lr_selec.



  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
FORM f_popup_to_confirm USING p_question TYPE c
                     CHANGING p_answer TYPE c.

  CALL FUNCTION 'POPUP_TO_CONFIRM'
    EXPORTING
      titlebar       = sy-title
      text_question  = p_question
    IMPORTING
      answer         = p_answer
    EXCEPTIONS
      text_not_found = 1
      OTHERS         = 2.

  IF sy-subrc <> 0.
    PERFORM f_mensagem_sistema.
  ENDIF.

ENDFORM.                    " F_POPUP_TO_CONFIRM
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_set_alv_status USING p_type TYPE bapi_mtype
                            p_string TYPE string
                   CHANGING p_dados_alv TYPE zsds066.

  CASE p_type.
    WHEN 'I' OR 'W'.
      p_dados_alv-icon = icon_yellow_light.
    WHEN 'S'.
      p_dados_alv-icon = icon_green_light.
    WHEN 'E'.
      p_dados_alv-icon = icon_red_light.
  ENDCASE.

  p_dados_alv-msgtx = p_string.

  PERFORM f_mensagem_insere_txt
    USING p_type
          p_string.

ENDFORM.
*&---------------------------------------------------------------------*
*&      FORM  F_MENSAGEM_INSERE_TXT
*&---------------------------------------------------------------------*
FORM f_mensagem_insere_txt USING i_type TYPE bapi_mtype
                                 p_string TYPE string.

  DATA: lt_trtexts     TYPE trtexts,
        lw_trtexts     TYPE trtext,
        lv_texto(4000).

  DATA lv_msg1 TYPE sy-msgv1.
  DATA lv_msg2 TYPE sy-msgv1.
  DATA lv_msg3 TYPE sy-msgv1.
  DATA lv_msg4 TYPE sy-msgv1.

  lv_texto = p_string.

  CALL FUNCTION 'TR_SPLIT_TEXT'
    EXPORTING
      iv_text  = lv_texto
      iv_len   = 30
    IMPORTING
      et_lines = lt_trtexts.

  LOOP AT lt_trtexts ASSIGNING FIELD-SYMBOL(<fs_line>).

    CASE sy-tabix.
      WHEN 1.
        lv_msg1 = <fs_line>.
      WHEN 2.
        lv_msg2 = <fs_line>.
      WHEN 3.
        lv_msg3 = <fs_line>.
      WHEN 4.
        lv_msg4 = <fs_line>.
    ENDCASE.

  ENDLOOP.

  PERFORM f_mensagem_insere
     USING i_type
           'DS'
           '016'
           lv_msg1
           lv_msg2
           lv_msg3
           lv_msg4.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCESSA_SELEC_INICIAL
*&---------------------------------------------------------------------*
FORM f_proce_inicial.

  DATA lv_refkey TYPE j_1brefkey.

  PERFORM f_sort_tabelas.

  DATA: lva_bukrs       TYPE j_1bbranch-bukrs,
        lva_nr_romaneio TYPE  zsdt0001-nr_romaneio,
        lva_branch      TYPE j_1bbranch-branch.

  DATA: lwa_tvrot  TYPE tvrot.

  LOOP AT gt_vfkp ASSIGNING FIELD-SYMBOL(<fs_vfkp>).

    CLEAR: lva_bukrs, lva_branch, lwa_tvrot.

    READ TABLE gt_vttk ASSIGNING FIELD-SYMBOL(<fs_vttk>)
     WITH KEY tknum = <fs_vfkp>-rebel BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_tvrot INTO lwa_tvrot
      WITH KEY route = <fs_vttk>-route BINARY SEARCH.


    " 2.3.13  Busca empresa/filial a partir da Organização de transporte
    READ TABLE gt_branch ASSIGNING FIELD-SYMBOL(<fs_branch>)
      WITH KEY branch = <fs_vttk>-tplst BINARY SEARCH.

    IF sy-subrc EQ 0.
      lva_bukrs   = <fs_branch>-bukrs.
      lva_branch  = <fs_branch>-branch.
    ELSE.
      READ TABLE gt_zcentro ASSIGNING FIELD-SYMBOL(<fs_zcentro>)
        WITH KEY centrov_1 = <fs_vttk>-tplst BINARY SEARCH.

      IF sy-subrc EQ 0.
        lva_bukrs   = <fs_zcentro>-vkorg.
        lva_branch  = <fs_zcentro>-centro_real.
      ENDIF.
    ENDIF.

    CHECK lva_bukrs IN r_bukrs_pnl.


    READ TABLE gt_tvtk ASSIGNING FIELD-SYMBOL(<fs_tvtk>)
      WITH KEY shtyp =  <fs_vttk>-shtyp BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_tvtkt ASSIGNING FIELD-SYMBOL(<fs_tvtkt>)
      WITH KEY shtyp =  <fs_vttk>-shtyp BINARY SEARCH.

    CHECK sy-subrc EQ 0.

    LOOP AT gt_vbfa_vt ASSIGNING FIELD-SYMBOL(<fs_vbfa_vt>) WHERE vbeln = <fs_vttk>-tknum.   "Fornecimentos da VT

      LOOP AT gt_lips ASSIGNING FIELD-SYMBOL(<fs_lips>) WHERE vbeln = <fs_vbfa_vt>-vbelv.

        READ TABLE gt_likp ASSIGNING FIELD-SYMBOL(<fs_likp>)
          WITH KEY vbeln = <fs_lips>-vbeln BINARY SEARCH.

        CHECK sy-subrc EQ 0.

        APPEND INITIAL LINE TO gt_dados_alv ASSIGNING FIELD-SYMBOL(<fs_alv>).

        <fs_alv>-xempresa   = lva_bukrs.
        <fs_alv>-xfilial    = lva_branch.


        " 2.3.1  Seleção inicial
        <fs_alv>-vbfa_vbelv = <fs_likp>-vbeln.
        <fs_alv>-tknum      = <fs_vttk>-tknum.
        <fs_alv>-erdat      = <fs_vttk>-erdat.
        <fs_alv>-shtyp      = <fs_vttk>-shtyp.
        <fs_alv>-tdlnr      = <fs_vttk>-tdlnr.
        <fs_alv>-route      = <fs_vttk>-route.
        <fs_alv>-tplst      = <fs_vttk>-tplst.

        " 2.3.2 Buscar a classificação do tipo de transporte
        <fs_alv>-abfer = <fs_tvtk>-abfer.
        <fs_alv>-vsart = <fs_tvtk>-vsart.
        <fs_alv>-bezei = <fs_tvtkt>-bezei.

        " 2.3.3 Buscar os documentos de faturamento (remessas e avisos) vinculados as VTs (doc de transporte)
        <fs_alv>-vbtyp_v = <fs_vbfa_vt>-vbtyp_v.

        " 2.3.4 Buscar dados da LIKP
        <fs_alv>-btgew = <fs_likp>-btgew.
        <fs_alv>-gewei = <fs_likp>-gewei.
        <fs_alv>-vbtyp = <fs_likp>-vbtyp.

        <fs_alv>-xblnr = <fs_likp>-xblnr.

        IF <fs_likp>-lifex IS NOT INITIAL.
          <fs_alv>-lifex = <fs_likp>-lifex.
        ELSE.
          <fs_alv>-lifex = <fs_likp>-xblnr.
        ENDIF.

        <fs_alv>-tcode = <fs_likp>-tcode.

        IF <fs_alv>-lifex CA '-'.

          SPLIT <fs_alv>-lifex AT '-'  INTO <fs_alv>-xnfenum <fs_alv>-xserie.

          UNPACK <fs_alv>-xnfenum TO <fs_alv>-xnfnum.
          UNPACK <fs_alv>-xnfenum TO <fs_alv>-znfnum.
          UNPACK <fs_alv>-xnfenum TO <fs_alv>-xnfenum.
          UNPACK <fs_alv>-xserie TO <fs_alv>-xserie.

        ENDIF.

        <fs_alv>-vbeln_vl   = <fs_lips>-vbeln.
        <fs_alv>-posnr_vl   = <fs_lips>-posnr.
        <fs_alv>-matnr      = <fs_lips>-matnr.
        <fs_alv>-matkl      = <fs_lips>-matkl.
        <fs_alv>-charg      = <fs_lips>-charg.
        <fs_alv>-werks      = <fs_lips>-werks.

        IF <fs_alv>-charg CA '_'.
          SPLIT <fs_alv>-charg AT '_'  INTO <fs_alv>-xsafra <fs_alv>-xfilial.
        ENDIF.

        <fs_alv>-vgbel = <fs_lips>-vgbel.

        READ TABLE gt_ekko INTO DATA(lw_ekko) WITH KEY ebeln = <fs_lips>-vgbel BINARY SEARCH.
        IF sy-subrc eq 0 AND lw_ekko-bsart = 'ZARM'.
          <fs_alv>-del_registro = abap_true.
        ENDIF.

        " 2.3.6 Buscar os valores de frete
        IF <fs_vfkp> IS ASSIGNED.

          <fs_alv>-vfkp_fknum = <fs_vfkp>-fknum.
          <fs_alv>-vfkp_knumv = <fs_vfkp>-knumv.
          <fs_alv>-vfkp_waers = <fs_vfkp>-waers.
          <fs_alv>-vfkp_kzwi1 = <fs_vfkp>-kzwi1.
          <fs_alv>-vfkp_prsdt = <fs_vfkp>-prsdt.

          READ TABLE gt_vfsi ASSIGNING FIELD-SYMBOL(<fs_vfsi>)
            WITH KEY knumv = <fs_vfkp>-knumv
                     vbeln = <fs_lips>-vbeln
                     posnr = <fs_lips>-posnr BINARY SEARCH.

          IF sy-subrc EQ 0.

            <fs_vfsi>-kzwi1 = <fs_vfsi>-kwert_zfre.

            IF <fs_vfsi>-kzwi1 IS INITIAL. "VI sem Valor
              <fs_alv>-del_registro = abap_true.
            ENDIF.

            IF <fs_vfkp>-kbert_zfre NE 0.
              <fs_alv>-vfsi_xqtde_forn   =  <fs_vfsi>-kzwi1 / <fs_vfkp>-kbert_zfre.

              IF <fs_vfsi>-kmein = 'TO'.
                <fs_alv>-vfsi_xqtde_forn =  <fs_alv>-vfsi_xqtde_forn * 1000.
              ENDIF.
            ENDIF.

            <fs_alv>-vfsi_vbeln = <fs_vfsi>-vbeln.
            <fs_alv>-vfsi_kzwi1 = <fs_vfsi>-kzwi1.
            <fs_alv>-vfsi_netpr = <fs_vfsi>-netpr.
            <fs_alv>-vfsi_kmein = <fs_vfsi>-kmein.

          ELSE.
            <fs_alv>-del_registro = abap_true.
          ENDIF.

          IF <fs_vfkp>-waers = 'BRL'.

            READ TABLE gt_curr_brl ASSIGNING FIELD-SYMBOL(<fs_curr_brl>)
              WITH KEY gdatu = <fs_vfkp>-gdatu BINARY SEARCH.

            IF sy-subrc EQ 0.

              <fs_alv>-ukurs = <fs_curr_brl>-ukurs.

              IF <fs_alv>-ukurs < 0.

                <fs_alv>-xusd = <fs_vfsi>-kzwi1 / <fs_alv>-ukurs.
                <fs_alv>-xbrl = <fs_vfsi>-kzwi1.

                <fs_alv>-vlr_pedagio_usd = <fs_vfsi>-kwert_zped / <fs_alv>-ukurs.
                <fs_alv>-vlr_pedagio_brl = <fs_vfsi>-kwert_zped.

                <fs_alv>-xusd            = abs( <fs_alv>-xusd ).
                <fs_alv>-vlr_pedagio_usd = abs( <fs_alv>-vlr_pedagio_usd ).

              ELSE.

                <fs_alv>-xusd = <fs_vfsi>-kzwi1 * <fs_alv>-ukurs.
                <fs_alv>-xbrl = <fs_vfsi>-kzwi1.

                <fs_alv>-vlr_pedagio_usd = <fs_vfsi>-kwert_zped * <fs_alv>-ukurs.
                <fs_alv>-vlr_pedagio_brl = <fs_vfsi>-kwert_zped.

              ENDIF.

            ELSE.

              lv_mess = 'Sem cotação para o dia: ' && <fs_vfkp>-prsdt.
              PERFORM f_set_alv_status USING 'E' lv_mess CHANGING <fs_alv>.

            ENDIF.

          ELSEIF <fs_vfkp>-waers = 'USD'.

            READ TABLE gt_curr_usd ASSIGNING FIELD-SYMBOL(<fs_curr_usd>)
              WITH KEY gdatu = <fs_vfkp>-gdatu BINARY SEARCH.

            IF sy-subrc EQ 0.

              <fs_alv>-ukurs = <fs_curr_usd>-ukurs.

              IF <fs_alv>-ukurs < 0.

                <fs_alv>-xbrl = <fs_vfsi>-kzwi1 / <fs_alv>-ukurs.
                <fs_alv>-xusd = <fs_vfsi>-kzwi1.

                <fs_alv>-vlr_pedagio_brl  = <fs_vfsi>-kwert_zped / <fs_alv>-ukurs.
                <fs_alv>-vlr_pedagio_usd  = <fs_vfsi>-kwert_zped.

                <fs_alv>-xbrl            = abs( <fs_alv>-xbrl ).
                <fs_alv>-vlr_pedagio_brl = abs( <fs_alv>-vlr_pedagio_brl ).

              ELSE.

                <fs_alv>-xbrl = <fs_vfsi>-kzwi1 * <fs_alv>-ukurs.
                <fs_alv>-xusd = <fs_vfsi>-kzwi1.

                <fs_alv>-vlr_pedagio_brl     = <fs_vfsi>-kwert_zped * <fs_alv>-ukurs.
                <fs_alv>-vlr_pedagio_usd     = <fs_vfsi>-kwert_zped.

              ENDIF.

            ELSE.

              lv_mess = 'Sem cotação para o dia: ' && <fs_vfkp>-prsdt.
              PERFORM f_set_alv_status USING 'E' lv_mess CHANGING zsds066.

            ENDIF.

          ENDIF.

        ENDIF.

        " 2.3.7  Buscar descritivo do Itinerário

        <fs_alv>-bezei_txt = lwa_tvrot-bezei.

        " 2.3.8  Buscar Docnum e NF vinculados ao transporte
        IF <fs_alv>-vbtyp_v = 'J'.

          READ TABLE gt_vbfa_nf ASSIGNING FIELD-SYMBOL(<fs_vbfa_nf>)
            WITH KEY vbelv =  <fs_vbfa_vt>-vbelv BINARY SEARCH.

          IF sy-subrc EQ 0.

            CLEAR lv_refkey.

            <fs_alv>-vbfa_vbeln = <fs_vbfa_nf>-vbeln.

            IF <fs_vbfa_nf>-vbtyp_n = 'M'.

              lv_refkey = <fs_vbfa_nf>-vbeln.

            ELSEIF <fs_vbfa_nf>-vbtyp_n = 'R'.

              lv_refkey = <fs_vbfa_nf>-vbeln && <fs_vbfa_nf>-erdat(4).

            ENDIF.

            READ TABLE gt_lin ASSIGNING FIELD-SYMBOL(<fs_lin>)
              WITH KEY refkey = lv_refkey BINARY SEARCH.

            IF sy-subrc EQ 0.

              READ TABLE gt_doc ASSIGNING FIELD-SYMBOL(<fs_doc>)
                WITH KEY docnum = <fs_lin>-docnum BINARY SEARCH.

              IF sy-subrc EQ 0.

                <fs_alv>-docnum = <fs_doc>-docnum.
                <fs_alv>-nfenum = <fs_doc>-nfenum.

              ENDIF.


            ENDIF.

            " 2.3.9 - Buscar parceiro Z1 das OVs ZRFL, ZDCO ou ZIND
            READ TABLE gt_vbak ASSIGNING FIELD-SYMBOL(<fs_vbak>)
             WITH KEY vbeln = <fs_lips>-vgbel BINARY SEARCH.

            IF sy-subrc EQ 0.

              READ TABLE gt_vbpa ASSIGNING FIELD-SYMBOL(<fs_vbpa>)
                WITH KEY vbeln = <fs_vbak>-vbeln
                         parvw = 'Z1' BINARY SEARCH.

              IF sy-subrc EQ 0.

                READ TABLE gt_lfa1 ASSIGNING FIELD-SYMBOL(<fs_lfa1>)
                  WITH KEY lifnr = <fs_vbpa>-lifnr BINARY SEARCH.

                IF sy-subrc EQ 0.
                  <fs_alv>-lifnr_z1 = <fs_vbpa>-lifnr.
                  <fs_alv>-lfa1_name1 = <fs_lfa1>-name1.
                ENDIF.

              ENDIF.

            ENDIF.

          ENDIF.

        ELSEIF <fs_alv>-vbtyp_v = '7'.

          READ TABLE gt_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zdocs>)
            WITH KEY av_vbeln = <fs_vbfa_vt>-vbelv BINARY SEARCH.

          IF sy-subrc EQ 0.

            READ TABLE gt_doc ASSIGNING <fs_doc>
              WITH KEY docnum = <fs_zdocs>-docnum BINARY SEARCH.

            IF sy-subrc EQ 0.

              <fs_alv>-docnum = <fs_zdocs>-docnum.
              <fs_alv>-nfenum = <fs_doc>-nfenum.

            ENDIF.

          ENDIF.

          " 2.3.11 - selecionar o parceiro LF do aviso de recebimento
          READ TABLE gt_vbpa_lf ASSIGNING FIELD-SYMBOL(<fs_vbpa_lf>)
            WITH KEY vbeln = <fs_vbfa_vt>-vbelv BINARY SEARCH.

          IF sy-subrc EQ 0.
            <fs_alv>-lifnr_lf = <fs_vbpa_lf>-lifnr.
          ENDIF.

        ENDIF.

        " 2.3.10 - Verificar se tem carta de correção para corrigir o parceiro Z1
        IF <fs_alv>-docnum IS NOT INITIAL.

          READ TABLE gt_carta ASSIGNING FIELD-SYMBOL(<fs_carta>)
            WITH KEY docnum = <fs_alv>-docnum BINARY SEARCH.

          IF sy-subrc EQ 0.

            <fs_alv>-novo_terminal = <fs_carta>-novo_terminal.
            <fs_alv>-dt_authcod = <fs_carta>-dt_authcod.
            <fs_alv>-hr_authcod = <fs_carta>-hr_authcod.

          ENDIF.

        ENDIF.

        " 2.3.12 - Selecione os parceiros do transporte
        LOOP AT gt_vtpa ASSIGNING FIELD-SYMBOL(<fs_vtpa>)
          WHERE vbeln = <fs_vttk>-tknum.

          CASE <fs_vtpa>-parvw.
            WHEN 'PC'.
              <fs_alv>-lifnr_pc = <fs_vtpa>-lifnr.
            WHEN 'PV'.
              <fs_alv>-lifnr_pv = <fs_vtpa>-lifnr.
            WHEN 'SP'.
              <fs_alv>-lifnr_sp = <fs_vtpa>-lifnr.
            WHEN 'LR'.
              <fs_alv>-kunnr_lr = <fs_vtpa>-kunnr.
          ENDCASE.

        ENDLOOP.

      ENDLOOP.

    ENDLOOP. "LOOP AT gt_vbfa_vt ASSIGNING FIELD-SYMBOL(<fs_vbfa_vt>) WHERE vbeln = <fs_vttk>-tknum.

*** Comentei Camila Brand
*** Adicionei essa verificação no select da lips
***    PERFORM f_check_stvarv CHANGING <fs_alv>.
**** Comentei Camila Brand
  ENDLOOP.

  DELETE gt_dados_alv WHERE del_registro EQ abap_true.
  DELETE gt_dados_alv WHERE xempresa NOT IN r_bukrs_pnl.

  IF gt_dados_alv[] IS NOT INITIAL.
    SELECT a~* INTO CORRESPONDING FIELDS OF TABLE @gt_vttp_rodo
      FROM vttp AS a INNER JOIN vttk AS b ON a~tknum = b~tknum
      FOR ALL ENTRIES IN @gt_dados_alv
     WHERE a~vbeln EQ @gt_dados_alv-vbeln_vl
       AND b~vsart EQ '01'. "Rodoviario


    SELECT * FROM zsdt0001
      INTO TABLE gt_xrom_saida
       FOR ALL ENTRIES IN gt_dados_alv
         WHERE tp_movimento  = 'S'
           AND doc_rem  = gt_dados_alv-vbeln_vl.

    "1.1.1.1 - Busca ID_Transporte do romaneio de entrada vinculado a romaneio de saída
    LOOP AT gt_xrom_saida ASSIGNING FIELD-SYMBOL(<fs_rom_saida>).
      IF <fs_rom_saida>-id_referencia IS NOT INITIAL.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = <fs_rom_saida>-id_referencia
          IMPORTING
            output = lva_nr_romaneio.

        <fs_rom_saida>-id_referencia = lva_nr_romaneio.

        SELECT SINGLE *
          FROM zsdt0001 INTO @DATA(lwa_xrom_entrada)
            WHERE tp_movimento EQ 'E'
              AND branch       EQ @<fs_rom_saida>-branch
              AND bukrs        EQ @<fs_rom_saida>-bukrs
              AND nr_safra     EQ @<fs_rom_saida>-nr_safra
              AND nr_romaneio  EQ @lva_nr_romaneio.

        IF sy-subrc EQ 0.
          APPEND lwa_xrom_entrada TO gt_xrom_entrada.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GET_STVARVS
*&---------------------------------------------------------------------*
FORM f_get_stvarvs.

  SELECT * FROM tvarvc
    INTO TABLE gt_matkl_pnl
    WHERE name = 'MATKL_P&L_FRETE'.


  LOOP AT gt_matkl_pnl INTO DATA(w_matkl_pnl).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_matkl_pnl-low ) TO r_matkl_pnl.
  ENDLOOP.

  APPEND VALUE #( sign = 'I' option = 'EQ' low = '999999' ) TO r_matkl_pnl.

  "Algodao ficará na fase 2 do projeto
*  SELECT * FROM tvarvc
*    INTO TABLE gt_maggi_algodao
*     WHERE name = 'MAGGI_GR_ALGODAO'.
*
*  LOOP AT gt_maggi_algodao INTO DATA(w_maggi_algodao).
*    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_maggi_algodao-low ) TO r_matkl_pnl.
*  ENDLOOP.

  SELECT * FROM tvarvc
    INTO TABLE gt_bukrs_pnl
     WHERE name = 'EMPRESA_P&L_FRETE'.

  LOOP AT gt_bukrs_pnl INTO DATA(w_bukrs_pnl).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_bukrs_pnl-low ) TO r_bukrs_pnl.
  ENDLOOP.

  SELECT * FROM tvarvc
    INTO TABLE gt_auart_rodo_s
    WHERE name = 'MAGGI_AU_RS'.

  LOOP AT gt_auart_rodo_s INTO DATA(w_auart_rodo_s).
    APPEND VALUE #( sign = 'I' option = 'EQ' low = w_auart_rodo_s-low ) TO r_auart_rs.
  ENDLOOP.

  SELECT SINGLE low FROM tvarvc
    INTO @DATA(lv_dias_txt)
      WHERE name = 'ZSDR0129_DIAS_BUSCA_ALT_FRETE'.

  IF lv_dias_txt CA '0123456789'.
    gv_dias_busca = lv_dias_txt.
  ELSE.
    CLEAR gv_dias_busca.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_CHECK_STVARV
*&---------------------------------------------------------------------*
*FORM f_check_stvarv CHANGING p_dados_alv TYPE zsds066.
*
*  DATA lv_existe TYPE flag.
*
*  IF p_dados_alv-matkl IS NOT INITIAL.
*
*    READ TABLE gt_matkl_pnl TRANSPORTING NO FIELDS
*      WITH KEY low = p_dados_alv-matkl.
*
*    IF sy-subrc = 0.
*      lv_existe = 'X'.
*    ENDIF.
*
*    READ TABLE gt_maggi_algodao TRANSPORTING NO FIELDS
*      WITH KEY low = p_dados_alv-matkl.
*
*    IF sy-subrc = 0.
*      lv_existe = 'X'.
*    ENDIF.
*
*    IF lv_existe IS INITIAL.
*
*      lv_mess = `Grp.Mercad: ` && p_dados_alv-matkl && ` não existe nas STVARV`.
*
*      PERFORM f_set_alv_status USING 'E' lv_mess CHANGING p_dados_alv.
*
*    ENDIF.
*
*  ENDIF.
*
*ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_selec_dados_trecho_ent
*&---------------------------------------------------------------------*
FORM f_selec_dados_trecho_ent .

  DATA: lva_ok TYPE c.

  "# DEBUG - RAMON -->
  "DELETE gt_dados_alv WHERE icon = icon_red_light.
  "# DEBUG - RAMON --<

  CHECK gt_dados_alv IS NOT INITIAL.

  gt_dados_ent_rodo = gt_dados_alv.

  DELETE gt_dados_ent_rodo WHERE abfer <> 2.
  DELETE gt_dados_ent_rodo WHERE vsart <> '01'.

  CHECK gt_dados_ent_rodo[] IS NOT INITIAL.

  " 3.2.1.1. - Busca de romaneio de entrada

  " Via nota fiscal do produtor  (primeira forma de busca)
  SELECT * FROM zsdt0001
    INTO TABLE gt_001_ent
      FOR ALL ENTRIES IN gt_dados_ent_rodo
        WHERE tp_movimento  = 'E'
          AND parid  = gt_dados_ent_rodo-lifnr_lf
          AND nfnum  = gt_dados_ent_rodo-znfnum
          AND series = gt_dados_ent_rodo-xserie.

  "AND branch = gt_dados_ent_rodo-xfilial
  "AND nr_safra = gt_dados_ent_rodo-xsafra.

  " Via nota fiscal própria  ( caso não encontrar o romaneio pela primeira forma de busca)

  SELECT * FROM j_1bnfdoc
    INTO TABLE gt_doc_nota_p
      FOR ALL ENTRIES IN gt_dados_ent_rodo
        WHERE direct = '1'
          AND form   <> ' '
          AND nfenum = gt_dados_ent_rodo-xnfenum
          AND series = gt_dados_ent_rodo-xserie
          AND branch = gt_dados_ent_rodo-xfilial
          AND parid  = gt_dados_ent_rodo-lifnr_lf.

  IF gt_doc_nota_p[] IS NOT INITIAL.

    SELECT z32~obj_key z32~itmnum
           z32~refnfe z32~cuf z32~aamm
           z32~cnpj z32~mod z32~serie
           z32~nnf z32~cpf z32~ie z32~refcte z31~docnum
      FROM zsdt0231 AS z31
      INNER JOIN zsdt0232 AS z32 ON z31~obj_key = z32~obj_key
      INTO TABLE gt_zsdt0232
      FOR ALL ENTRIES IN gt_doc_nota_p
        WHERE docnum = gt_doc_nota_p-docnum.

    IF gt_zsdt0232[] IS NOT INITIAL.

      SELECT * FROM zsdt0001
        APPENDING TABLE gt_001_ent
          FOR ALL ENTRIES IN gt_zsdt0232
            WHERE tp_movimento  = 'E'
              AND nfnum  = gt_zsdt0232-nnf
              AND series = gt_zsdt0232-serie.

      SELECT * FROM zsdt0001
         APPENDING TABLE gt_001_ent
           FOR ALL ENTRIES IN gt_zsdt0232
             WHERE tp_movimento  = 'E'
               AND chave_nfe  = gt_zsdt0232-refnfe.

    ENDIF.

  ENDIF.

  " Via interface do grãos ( quando não encontrar pela primeira ou segunda forma de busca)
  IF gt_zgr_docs[] IS NOT INITIAL.

    SELECT * FROM zmmt_ee_zgr
      INTO TABLE gt_zgr
        FOR ALL ENTRIES IN gt_zgr_docs
          WHERE obj_key = gt_zgr_docs-obj_key.

    IF gt_zgr[] IS NOT INITIAL.

      SELECT * FROM zsdt0001
      APPENDING TABLE gt_001_ent
        FOR ALL ENTRIES IN gt_zgr
          WHERE ch_referencia =  gt_zgr-ch_referencia.

    ENDIF.

  ENDIF.

  SORT gt_001_ent BY ch_referencia.
  DELETE ADJACENT DUPLICATES FROM gt_001_ent COMPARING ch_referencia.

  IF gt_001_ent[] IS NOT INITIAL. "Identificação de Romaneios Desmembrados

    DATA(gt_001_ent_aux) = gt_001_ent[].
    DELETE gt_001_ent_aux WHERE ch_refer_ent IS INITIAL.

    IF gt_001_ent_aux[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001 INTO TABLE gt_001_ent_desmemb
        FOR ALL ENTRIES IN gt_001_ent_aux
       WHERE ch_refer_ent = gt_001_ent_aux-ch_refer_ent.
    ENDIF.


    gt_001_ent_aux = gt_001_ent[].
    DELETE gt_001_ent_aux WHERE id_carga IS INITIAL.

    IF gt_001_ent_aux[] IS NOT INITIAL.
      SELECT *
        FROM zsdt0001 APPENDING TABLE gt_001_ent_desmemb
        FOR ALL ENTRIES IN gt_001_ent_aux
       WHERE id_carga     = gt_001_ent_aux-id_carga.
    ENDIF.

    DELETE gt_001_ent_desmemb WHERE tp_movimento NE 'E'.

*    IF gt_001_ent_desmemb[] is NOT INITIAL.
*      SELECT *
*        from zlest0223 INTO TABLE gt_223_ent_desmemb
*        FOR ALL ENTRIES IN gt_001_ent_desmemb
*       WHERE ch_referencia_rom = gt_001_ent_desmemb-ch_referencia.
*    ENDIF.

  ENDIF.


  LOOP AT gt_001_ent ASSIGNING FIELD-SYMBOL(<fs_0001_ent>).

    DATA(lva_tabix) = sy-tabix.

    IF <fs_0001_ent>-id_carga IS NOT INITIAL.
      CLEAR: <fs_0001_ent>-ch_refer_ent.
    ENDIF.

  ENDLOOP.

  LOOP AT gt_001_ent_desmemb ASSIGNING FIELD-SYMBOL(<fs_ent_desmemb>).

    lva_tabix = sy-tabix.

    IF <fs_ent_desmemb>-id_carga IS NOT INITIAL.
      CLEAR: <fs_ent_desmemb>-ch_refer_ent.
    ENDIF.

  ENDLOOP.




ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  f_proce_dados_trecho_ent
*&---------------------------------------------------------------------*
FORM f_proce_dados_trecho_ent.

  DATA: lva_ok TYPE c.

  FIELD-SYMBOLS <fs_zsd001> TYPE zsdt0001.

  LOOP AT gt_dados_ent_rodo ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE abfer = 2.

    UNASSIGN <fs_zsd001>.

    " 3.2.1.1. - Busca de romaneio de entrada

    " Via nota fiscal do produtor  (primeira forma de busca)
    READ TABLE gt_001_ent ASSIGNING <fs_zsd001>
      WITH KEY nfnum    = <fs_dados>-znfnum
               series   = <fs_dados>-xserie
               parid    = <fs_dados>-lifnr_lf
               branch   = <fs_dados>-xfilial.

    IF sy-subrc NE 0.

      " Via nota fiscal própria  ( caso não encontrar o romaneio pela primeira forma de busca)
      READ TABLE gt_doc_nota_p ASSIGNING FIELD-SYMBOL(<fs_doc_p>)
        WITH KEY nfenum = <fs_dados>-xnfenum
                 series = <fs_dados>-xserie
                 branch = <fs_dados>-xfilial
                 parid  = <fs_dados>-lifnr_lf.

      IF sy-subrc EQ 0.

        READ TABLE gt_zsdt0232 ASSIGNING FIELD-SYMBOL(<fs_zsdt0232>)
          WITH KEY docnum = <fs_doc_p>-docnum.

        IF sy-subrc EQ 0.

          IF <fs_zsdt0232>-refnfe IS NOT INITIAL.
            READ TABLE gt_001_ent ASSIGNING <fs_zsd001>
              WITH KEY chave_nfe  = <fs_zsdt0232>-refnfe.
          ELSE.
            READ TABLE gt_001_ent ASSIGNING <fs_zsd001>
              WITH KEY nfnum  = <fs_zsdt0232>-nnf
                       series = <fs_zsdt0232>-serie.
          ENDIF.

        ENDIF.

      ENDIF.

      " Via interface do grãos ( quando não encontrar pela primeira ou segunda forma de busca)
      READ TABLE gt_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zdocs>)
        WITH KEY av_vbeln = <fs_dados>-vbeln_vl BINARY SEARCH.

      IF sy-subrc EQ 0.

        READ TABLE gt_zgr ASSIGNING FIELD-SYMBOL(<fs_zgr>)
          WITH KEY obj_key = <fs_zdocs>-obj_key.

        IF sy-subrc EQ 0.

          READ TABLE gt_001_ent ASSIGNING <fs_zsd001>
            WITH KEY ch_referencia =  <fs_zgr>-ch_referencia.

        ENDIF.

      ENDIF.

    ENDIF.

    " atribuição -----

    IF <fs_zsd001> IS ASSIGNED.

      PERFORM f_check_hora_sinc_rom USING <fs_zsd001>
                                 CHANGING lva_ok.

      CHECK lva_ok EQ abap_true. "Só considerar o romaneio caso o mesmo já tiver sincronizado no SAP a mais de 2 horas

      IF ( <fs_zsd001>-ch_refer_ent IS NOT INITIAL ) OR ( <fs_zsd001>-id_carga IS NOT INITIAL ).

        LOOP AT gt_001_ent_desmemb INTO DATA(lwa_rom_desmemb) WHERE ch_refer_ent   EQ <fs_zsd001>-ch_refer_ent
                                                                AND id_carga       EQ <fs_zsd001>-id_carga
                                                                AND ch_referencia  NE <fs_zsd001>-ch_referencia.
          <fs_dados>-rom_desmembrado = abap_true.

          APPEND INITIAL LINE TO gt_dados_ent_rodo_desmemb ASSIGNING FIELD-SYMBOL(<fs_rom_ent_desmemb>).
          PERFORM f_transfere_dados_rom USING lwa_rom_desmemb
                                     CHANGING <fs_rom_ent_desmemb>.
        ENDLOOP.

        IF ( <fs_zsd001>-ch_refer_ent IS NOT INITIAL ) AND  "Romaneio Desmembrado via OPUS
           ( <fs_dados>-rom_desmembrado EQ abap_false  ).
          CONTINUE. "Não presseguir com o processamento, enquanto todos os romaneios de entrada desmembrados não chegarem no SAP
        ENDIF.
      ENDIF.

      PERFORM f_transfere_dados_rom USING <fs_zsd001>
                                 CHANGING <fs_dados>.

*      <fs_dados>-roma_ent_nr_rom        = <fs_zsd001>-nr_romaneio.
*      <fs_dados>-roma_ent_ch_ref        = <fs_zsd001>-ch_referencia.
*      <fs_dados>-roma_ent_branch        = <fs_zsd001>-branch.
*      <fs_dados>-roma_ent_safra         = <fs_zsd001>-nr_safra.
*      <fs_dados>-roma_ent_bukrs         = <fs_zsd001>-bukrs.
*      <fs_dados>-roma_ent_ch_refer_ent  = <fs_zsd001>-ch_refer_ent.
*      <fs_dados>-roma_ent_id_carga      = <fs_zsd001>-id_carga.
*
*
*      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
*        EXPORTING
*          input  = <fs_dados>-roma_ent_nr_rom
*        IMPORTING
*          output = <fs_dados>-roma_ent_nr_rom2.
*
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados>-roma_ent_nr_rom  ) TO <fs_dados>-roma_ent_nr_rom_r.
*      APPEND VALUE #( sign = 'I' option = 'EQ' low = <fs_dados>-roma_ent_nr_rom2 ) TO <fs_dados>-roma_ent_nr_rom_r.


    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_transfere_dados_rom USING p_zsdt0001  TYPE zsdt0001
                        CHANGING c_dados_rom TYPE zsds066.

  c_dados_rom-roma_ent_nr_rom        = p_zsdt0001-nr_romaneio.
  c_dados_rom-roma_ent_ch_ref        = p_zsdt0001-ch_referencia.
  c_dados_rom-roma_ent_branch        = p_zsdt0001-branch.
  c_dados_rom-roma_ent_safra         = p_zsdt0001-nr_safra.
  c_dados_rom-roma_ent_bukrs         = p_zsdt0001-bukrs.
  c_dados_rom-roma_ent_ch_refer_ent  = p_zsdt0001-ch_refer_ent.
  c_dados_rom-roma_ent_id_carga      = p_zsdt0001-id_carga.


  CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
    EXPORTING
      input  = c_dados_rom-roma_ent_nr_rom
    IMPORTING
      output = c_dados_rom-roma_ent_nr_rom2.

  APPEND VALUE #( sign = 'I' option = 'EQ' low = c_dados_rom-roma_ent_nr_rom  ) TO c_dados_rom-roma_ent_nr_rom_r.
  APPEND VALUE #( sign = 'I' option = 'EQ' low = c_dados_rom-roma_ent_nr_rom2 ) TO c_dados_rom-roma_ent_nr_rom_r.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELEC_DADOS_ROMANEIO
*&---------------------------------------------------------------------*
FORM f_selec_dados_rodo_ent .

  " 3.2.1.2 - Procurar romaneio de saída vinculado ao romaneio de entrada
  CHECK gt_dados_ent_rodo[] IS NOT INITIAL.

  SELECT * FROM zsdt0001
    INTO TABLE gt_001_ent_sai
    FOR ALL ENTRIES IN gt_dados_ent_rodo
      WHERE tp_movimento  = 'S'
        AND bukrs         =   gt_dados_ent_rodo-roma_ent_bukrs
        AND branch        =   gt_dados_ent_rodo-roma_ent_branch
        AND nr_safra      =   gt_dados_ent_rodo-roma_ent_safra
        AND id_referencia =   gt_dados_ent_rodo-roma_ent_nr_rom.

  SELECT * FROM zsdt0001
    APPENDING TABLE gt_001_ent_sai
    FOR ALL ENTRIES IN gt_dados_ent_rodo
      WHERE tp_movimento  = 'S'
        AND bukrs         =   gt_dados_ent_rodo-roma_ent_bukrs
        AND branch        =   gt_dados_ent_rodo-roma_ent_branch
        AND nr_safra      =   gt_dados_ent_rodo-roma_ent_safra
        AND id_referencia =   gt_dados_ent_rodo-roma_ent_nr_rom2.


  "Procurar romaneio de saída vinculado ao romaneio de entrada desmembrado
  IF gt_dados_ent_rodo_desmemb[] IS NOT INITIAL.
    SELECT * FROM zsdt0001
      APPENDING TABLE gt_001_ent_sai
      FOR ALL ENTRIES IN gt_dados_ent_rodo_desmemb
        WHERE tp_movimento  = 'S'
          AND bukrs         =   gt_dados_ent_rodo_desmemb-roma_ent_bukrs
          AND branch        =   gt_dados_ent_rodo_desmemb-roma_ent_branch
          AND nr_safra      =   gt_dados_ent_rodo_desmemb-roma_ent_safra
          AND id_referencia =   gt_dados_ent_rodo_desmemb-roma_ent_nr_rom.

    SELECT * FROM zsdt0001
      APPENDING TABLE gt_001_ent_sai
      FOR ALL ENTRIES IN gt_dados_ent_rodo_desmemb
        WHERE tp_movimento  = 'S'
          AND bukrs         =   gt_dados_ent_rodo_desmemb-roma_ent_bukrs
          AND branch        =   gt_dados_ent_rodo_desmemb-roma_ent_branch
          AND nr_safra      =   gt_dados_ent_rodo_desmemb-roma_ent_safra
          AND id_referencia =   gt_dados_ent_rodo_desmemb-roma_ent_nr_rom2.
  ENDIF.


  SORT gt_001_ent_sai BY ch_referencia.
  DELETE ADJACENT DUPLICATES FROM gt_001_ent_sai COMPARING ch_referencia.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_DADOS_ROMANEIO
*&---------------------------------------------------------------------*
FORM f_proce_dados_rodo_ent .

  LOOP AT gt_dados_ent_rodo ASSIGNING FIELD-SYMBOL(<fs_dados>).

    LOOP AT gt_001_ent_sai ASSIGNING FIELD-SYMBOL(<fs_romaneio>) WHERE bukrs         EQ <fs_dados>-roma_ent_bukrs
                                                                   AND branch        EQ <fs_dados>-roma_ent_branch
                                                                   AND nr_safra      EQ <fs_dados>-roma_ent_safra
                                                                   AND id_referencia IN <fs_dados>-roma_ent_nr_rom_r.
      <fs_dados>-roma_sai_doc_rem = <fs_romaneio>-doc_rem.
      <fs_dados>-roma_sai_tknum   = <fs_romaneio>-tknum.
      <fs_dados>-roma_sai_fknum   = <fs_romaneio>-fknum.
      <fs_dados>-roma_sai_ch_ref  = <fs_romaneio>-ch_referencia.
      <fs_dados>-roma_sai_branch  = <fs_romaneio>-branch.
      <fs_dados>-roma_sai_safra   = <fs_romaneio>-nr_safra.
      <fs_dados>-roma_sai_bukrs   = <fs_romaneio>-bukrs.

      EXIT.

    ENDLOOP.


    "Romaneio de Entrada Desmembrado deve ter os mesmo vinculos com romaneio de saida no caso de romaneio de completo
    IF ( <fs_dados>-rom_desmembrado EQ abap_true   ) AND
       ( <fs_dados>-roma_ent_ch_ref IS NOT INITIAL ) AND
       ( <fs_dados>-roma_sai_ch_ref IS INITIAL     ).

      LOOP AT gt_001_ent_desmemb INTO DATA(lwa_001_ent_desmemb) WHERE id_carga       EQ <fs_dados>-roma_ent_id_carga
                                                                  AND ch_refer_ent   EQ <fs_dados>-roma_ent_ch_refer_ent
                                                                  AND ch_referencia  NE <fs_dados>-roma_ent_ch_ref.

        READ TABLE gt_dados_ent_rodo_desmemb INTO DATA(lwa_rom_ent_desmemb) WITH KEY roma_ent_ch_ref = lwa_001_ent_desmemb-ch_referencia.
        CHECK sy-subrc EQ 0.
        LOOP AT gt_001_ent_sai ASSIGNING <fs_romaneio> WHERE bukrs         EQ lwa_rom_ent_desmemb-roma_ent_bukrs
                                                         AND branch        EQ lwa_rom_ent_desmemb-roma_ent_branch
                                                         AND nr_safra      EQ lwa_rom_ent_desmemb-roma_ent_safra
                                                         AND id_referencia IN lwa_rom_ent_desmemb-roma_ent_nr_rom_r.

          <fs_dados>-roma_sai_doc_rem = <fs_romaneio>-doc_rem.
          <fs_dados>-roma_sai_tknum   = <fs_romaneio>-tknum.
          <fs_dados>-roma_sai_fknum   = <fs_romaneio>-fknum.
          <fs_dados>-roma_sai_ch_ref  = <fs_romaneio>-ch_referencia.
          <fs_dados>-roma_sai_branch  = <fs_romaneio>-branch.
          <fs_dados>-roma_sai_safra   = <fs_romaneio>-nr_safra.
          <fs_dados>-roma_sai_bukrs   = <fs_romaneio>-bukrs.
          EXIT.
        ENDLOOP.
        IF <fs_dados>-roma_sai_ch_ref IS NOT INITIAL.
          EXIT.
        ENDIF.
      ENDLOOP.
    ENDIF.


  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_GRAVA_DADOS
*&---------------------------------------------------------------------*
FORM f_grava_dados_rodo_ent .

  DATA lt_zlest0223  TYPE TABLE OF zlest0223.
  DATA lwa_zlest0223 TYPE zlest0223.
  DATA lt_223_insert TYPE TABLE OF zlest0223.
  DATA lva_id_transp_desmb TYPE zlest0223-id_transporte.

  " 3.2.2. - Incluir os dados na tabela ZLEST02XX para fretes de entrada:
  CHECK gt_dados_ent_rodo[] IS NOT INITIAL.

  SELECT * FROM zlest0223
    INTO TABLE lt_zlest0223
    FOR ALL ENTRIES IN gt_dados_ent_rodo
      WHERE tknum             = gt_dados_ent_rodo-tknum
        AND fornecimento      = gt_dados_ent_rodo-vbeln_vl
        AND fornecimento_item = gt_dados_ent_rodo-posnr_vl
        AND fknum             = gt_dados_ent_rodo-vfkp_fknum.

  LOOP AT gt_dados_ent_rodo ASSIGNING FIELD-SYMBOL(<fs_dados_alv>).

    CLEAR: lwa_zlest0223.

    READ TABLE lt_zlest0223 INTO lwa_zlest0223
      WITH KEY tknum             = <fs_dados_alv>-tknum
               fornecimento      = <fs_dados_alv>-vbeln_vl
               fornecimento_item = <fs_dados_alv>-posnr_vl
               fknum             = <fs_dados_alv>-vfkp_fknum.

    CHECK ( sy-subrc NE 0 ).

    CHECK ( <fs_dados_alv>-roma_ent_ch_ref IS NOT INITIAL ). "Frete de Entrada só deve prosseguir com a gravação do registro de tiver localizado o Romaneio de Entrada

    "Frete de Entrada com Romaneio de Entrada Desmembrado deve ter o mesmo ID transporte
    IF ( <fs_dados_alv>-rom_desmembrado EQ abap_true ).
      LOOP AT gt_001_ent_desmemb INTO DATA(lwa_001_ent_desmemb) WHERE id_carga       EQ <fs_dados_alv>-roma_ent_id_carga
                                                                  AND ch_refer_ent   EQ <fs_dados_alv>-roma_ent_ch_refer_ent
                                                                  AND ch_referencia  NE <fs_dados_alv>-roma_ent_ch_ref.
        SELECT SINGLE *
          FROM zlest0223 INTO @DATA(lwa_223_desmb)
         WHERE ch_referencia_rom EQ @lwa_001_ent_desmemb-ch_referencia.

        IF sy-subrc EQ 0.
          lwa_zlest0223-id_transporte = lwa_223_desmb-id_transporte.
        ENDIF.
      ENDLOOP.
    ENDIF.

    IF lwa_zlest0223-id_transporte IS INITIAL.
      PERFORM f_get_id_transporte CHANGING lwa_zlest0223-id_transporte.
    ENDIF.


    lwa_zlest0223-trecho             = '1'.
    lwa_zlest0223-tp_trecho          = '01'.
    lwa_zlest0223-fornecimento       = <fs_dados_alv>-vbeln_vl.
    lwa_zlest0223-fornecimento_item  = <fs_dados_alv>-posnr_vl.
    lwa_zlest0223-porto_destino      = space.
    lwa_zlest0223-local_embarque     = <fs_dados_alv>-lifnr_pc.
    lwa_zlest0223-local_entrega      = <fs_dados_alv>-xfilial.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = lwa_zlest0223-local_entrega
      IMPORTING
        output = lwa_zlest0223-local_entrega.


    lwa_zlest0223-produto             = <fs_dados_alv>-matnr.
    lwa_zlest0223-empresa             = <fs_dados_alv>-xempresa.
    lwa_zlest0223-filial              = <fs_dados_alv>-xfilial.
    lwa_zlest0223-safra               = <fs_dados_alv>-xsafra.
    lwa_zlest0223-vbtyp_v             = <fs_dados_alv>-vbtyp.
    lwa_zlest0223-tknum               = <fs_dados_alv>-tknum.
    lwa_zlest0223-data_transporte     = <fs_dados_alv>-erdat.
    lwa_zlest0223-fknum               = <fs_dados_alv>-vfkp_fknum.
    lwa_zlest0223-data_custo          = <fs_dados_alv>-vfkp_prsdt.
    lwa_zlest0223-vsart               = <fs_dados_alv>-vsart.
    lwa_zlest0223-tdlnr               = <fs_dados_alv>-lifnr_sp.
    lwa_zlest0223-shtyp               = <fs_dados_alv>-shtyp.
    lwa_zlest0223-propr_veiculo       = <fs_dados_alv>-lifnr_pv.
    lwa_zlest0223-qtde_total          = <fs_dados_alv>-btgew.
    lwa_zlest0223-qtde_utilizada      = <fs_dados_alv>-btgew.
    lwa_zlest0223-unidade             = <fs_dados_alv>-gewei.
    lwa_zlest0223-chave_trecho_ref    = space.
    lwa_zlest0223-vl_brl              = <fs_dados_alv>-xbrl.
    lwa_zlest0223-vl_usd              = <fs_dados_alv>-xusd.
    lwa_zlest0223-vl_ped_brl          = <fs_dados_alv>-vlr_pedagio_brl.
    lwa_zlest0223-vl_ped_usd          = <fs_dados_alv>-vlr_pedagio_usd.
    lwa_zlest0223-ch_referencia_rom   = <fs_dados_alv>-roma_ent_ch_ref.

    " Devemos verificar se o romaneio de entrada esta vinculado a
    " algum romaneio de saída, conforme item 3.2.1.2
    IF <fs_dados_alv>-roma_sai_ch_ref IS NOT INITIAL.
      lwa_zlest0223-transbordo = abap_false.
    ELSE.
      lwa_zlest0223-transbordo = abap_true.
    ENDIF.

    IF lwa_zlest0223-transbordo EQ abap_true.
      lwa_zlest0223-saldo_transb = <fs_dados_alv>-btgew.
    ELSE.
      CLEAR: lwa_zlest0223-saldo_transb.
    ENDIF.

    lwa_zlest0223-realizado             = abap_true.
    lwa_zlest0223-nr_ov                 = space.
    lwa_zlest0223-nr_pedido             = <fs_dados_alv>-vgbel.
    lwa_zlest0223-cancelado             = abap_false.
    lwa_zlest0223-sincronizado_sigam    = space.
    lwa_zlest0223-user_reg              = sy-uname.
    lwa_zlest0223-data_reg              = sy-datum.
    lwa_zlest0223-hora_reg              = sy-uzeit.

    lv_mess = 'Inserido com sucesso'.

    PERFORM f_set_alv_status USING 'S' lv_mess CHANGING <fs_dados_alv>.

    MODIFY zlest0223 FROM lwa_zlest0223.

  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_DADOS_TRECHO_SAI
*&---------------------------------------------------------------------*
FORM f_selec_dados_trecho_sai .

  "# DEBUG - RAMON -->
  "DELETE gt_dados_alv WHERE icon = icon_red_light.
  "# DEBUG - RAMON --<

  " 3.2.1.1. - Busca de romaneio de entrada
* COMENTEI AQUI - CAMILA BRAND
*  " Via nota fiscal do produtor  (primeira forma de busca)
*  SELECT * FROM zsdt0001
*    INTO TABLE gt_001_sai
*      FOR ALL ENTRIES IN gt_dados_sai_rodo
*        WHERE tp_movimento  = 'S'
*          AND nfnum  = gt_dados_sai_rodo-znfnum
*          AND series = gt_dados_sai_rodo-xserie
*          AND parid  = gt_dados_sai_rodo-lifnr_lf
*          AND branch = gt_dados_sai_rodo-xfilial
*          AND nr_safra = gt_dados_sai_rodo-xsafra.
*
*  " Via nota fiscal própria  ( caso não encontrar o romaneio pela primeira forma de busca)
*
*  SELECT * FROM j_1bnfdoc
*    INTO TABLE gt_doc_nota_p
*      FOR ALL ENTRIES IN gt_dados_sai_rodo
*        WHERE direct = '1'
*          AND form   <> ' '
*          AND nfenum = gt_dados_sai_rodo-xnfenum
*          AND series = gt_dados_sai_rodo-xserie
*          AND branch = gt_dados_sai_rodo-xfilial
*          AND parid  = gt_dados_sai_rodo-lifnr_lf.
*
*  IF gt_doc_nota_p IS NOT INITIAL.
*
*    SELECT z32~obj_key z32~itmnum
*           z32~refnfe z32~cuf z32~aamm
*           z32~cnpj z32~mod z32~serie
*           z32~nnf z32~cpf z32~ie z32~refcte z31~docnum
*      FROM zsdt0231 AS z31
*      INNER JOIN zsdt0232 AS z32 ON z31~obj_key = z32~obj_key
*      INTO TABLE gt_zsdt0232
*      FOR ALL ENTRIES IN gt_doc_nota_p
*        WHERE docnum = gt_doc_nota_p-docnum.
*
*    IF sy-subrc EQ 0.
*
*      SELECT * FROM zsdt0001
*        APPENDING TABLE gt_001_sai
*          FOR ALL ENTRIES IN gt_zsdt0232
*            WHERE tp_movimento  = 'S'
*              AND nfnum  = gt_zsdt0232-nnf
*              AND series = gt_zsdt0232-serie.
*
*    ENDIF.
*
*  ENDIF.
*
*  " Via interface do grãos ( quando não encontrar pela primeira ou segunda forma de busca)
*  IF gt_zgr_docs IS NOT INITIAL.
*
*    SELECT * FROM zmmt_ee_zgr
*      INTO TABLE gt_zgr
*        FOR ALL ENTRIES IN gt_zgr_docs
*          WHERE obj_key = gt_zgr_docs-obj_key.
*
*    IF sy-subrc EQ 0.
*
*      SELECT * FROM zsdt0001
*      APPENDING TABLE gt_001_sai
*        FOR ALL ENTRIES IN gt_zgr
*          WHERE ch_referencia =  gt_zgr-ch_referencia.
*
*    ENDIF.
*
*  ENDIF.
* FIM AQUI -CAMILA BRAND


ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_DADOS_TRECHO_SAI
*&---------------------------------------------------------------------*
FORM f_proce_dados_trecho_sai .

*  LOOP AT gt_dados_sai_rodo ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE abfer = 1 AND vsart = '01'.
*
*    " 3.2.1.1. - Busca de romaneio de entrada
*
*    " Via nota fiscal do produtor  (primeira forma de busca)
*    READ TABLE gt_001_sai ASSIGNING FIELD-SYMBOL(<fs_zsd001>)
*      WITH KEY nfnum  = <fs_dados>-znfnum
*               series = <fs_dados>-xserie
*               parid  = <fs_dados>-lifnr_lf
*               branch = <fs_dados>-xfilial
*               nr_safra = <fs_dados>-xsafra.
*
*    IF sy-subrc NE 0.
*
*      " Via nota fiscal própria  ( caso não encontrar o romaneio pela primeira forma de busca)
*      READ TABLE gt_doc_nota_p ASSIGNING FIELD-SYMBOL(<fs_doc_p>)
*        WITH KEY nfenum = <fs_dados>-xnfenum
*                 series = <fs_dados>-xserie
*                 branch = <fs_dados>-xfilial
*                 parid  = <fs_dados>-lifnr_lf.
*
*      IF sy-subrc EQ 0.
*
*        READ TABLE gt_zsdt0232 ASSIGNING FIELD-SYMBOL(<fs_zsdt0232>)
*          WITH KEY docnum = <fs_doc_p>-docnum.
*
*        IF sy-subrc EQ 0.
*
*          READ TABLE gt_001_sai ASSIGNING <fs_zsd001>
*            WITH KEY nfnum  = <fs_zsdt0232>-nnf
*                     series = <fs_zsdt0232>-serie.
*
*        ENDIF.
*
*      ENDIF.
*
*      " Via interface do grãos ( quando não encontrar pela primeira ou segunda forma de busca)
*      READ TABLE gt_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zdocs>)
*        WITH KEY av_vbeln = <fs_dados>-vbelv.
*
*      IF sy-subrc EQ 0.
*
*        READ TABLE gt_zgr ASSIGNING FIELD-SYMBOL(<fs_zgr>)
*          WITH KEY obj_key = <fs_zdocs>-obj_key.
*
*        IF sy-subrc EQ 0.
*
*          READ TABLE gt_001_sai ASSIGNING <fs_zsd001>
*            WITH KEY ch_referencia =  <fs_zgr>-ch_referencia.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    " atribuição -----
*
*    IF <fs_zsd001> IS ASSIGNED.
*
*      <fs_dados>-roma_sai_ch_ref = <fs_zsd001>-ch_referencia.
*      <fs_dados>-roma_sai_branch = <fs_zsd001>-branch.
*      <fs_dados>-roma_sai_safra = <fs_zsd001>-nr_safra.
*      <fs_dados>-roma_sai_bukrs = <fs_zsd001>-bukrs.
*
*    ENDIF.
*
*  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_DADOS_ROMANEIO_SAI
*&---------------------------------------------------------------------*
FORM f_selec_dados_rodo_sai .

  DATA: lwa_xrom_saida   LIKE LINE OF gt_xrom_saida.
  DATA: lwa_xrom_entrada LIKE LINE OF gt_xrom_entrada.
  DATA: lva_nr_romaneio  TYPE  zsdt0001-nr_romaneio.
  DATA: lva_index        TYPE sy-tabix.

  CHECK gt_dados_alv IS NOT INITIAL.

*** US - 78597 - CBRAND - inicio
  gt_dados_sai_rodo = gt_dados_alv.

  DELETE gt_dados_sai_rodo WHERE abfer <> 1.
  DELETE gt_dados_sai_rodo WHERE vsart <> '01'.
  DELETE gt_dados_sai_rodo WHERE vbtyp_v <> 'J'.
*** US - 78597 - CBRAND - Fim

  CHECK gt_dados_sai_rodo[] IS NOT INITIAL.



  IF gt_xrom_entrada[] IS NOT INITIAL.
    SELECT * FROM zlest0223
      INTO TABLE gt_zlest0223
      FOR ALL ENTRIES IN gt_xrom_entrada
        WHERE ch_referencia_rom = gt_xrom_entrada-ch_referencia.
  ENDIF.

  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223_saida
    FOR ALL ENTRIES IN gt_dados_sai_rodo
      WHERE tknum              = gt_dados_sai_rodo-tknum
        AND fornecimento       = gt_dados_sai_rodo-vbeln_vl
        AND fornecimento_item  = gt_dados_sai_rodo-posnr_vl
        AND fknum              = gt_dados_sai_rodo-vfkp_fknum.




  CLEAR: gt_zlest0223_transb[].
  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223_transb
      FOR ALL ENTRIES IN gt_dados_sai_rodo
        WHERE empresa = gt_dados_sai_rodo-xempresa
          AND filial  = gt_dados_sai_rodo-xfilial
          AND transbordo    = abap_true
          AND cancelado <> abap_true   "Diferente de Cancelamento
          AND saldo_transb       > 0 .



* Comentei Camila Brand 06062022
*  " 3.2.1.2 - Procurar romaneio de saída vinculado ao romaneio de entrada
*  CHECK gt_dados_sai_rodo IS NOT INITIAL.
*
*  SELECT * FROM zsdt0001
*    INTO TABLE gt_001_ent_sai
*    FOR ALL ENTRIES IN gt_dados_sai_rodo
*      WHERE tp_movimento  = 'E'
*        AND bukrs         =   gt_dados_sai_rodo-roma_sai_bukrs
*        AND branch        =   gt_dados_sai_rodo-roma_sai_branch
*        AND nr_safra      =   gt_dados_sai_rodo-roma_sai_safra
*        AND id_referencia =   gt_dados_sai_rodo-roma_sai_ch_ref.
** Fim comentário Camila Brand

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_DADOS_ROMANEIO_SAI
*&---------------------------------------------------------------------*
FORM f_grava_dados_rodo_sai .

  DATA: lva_id_fiscal_pc  TYPE c LENGTH 16,
        lva_id_fiscal_lr  TYPE c LENGTH 16,
        lva_id_fiscal_z1  TYPE c LENGTH 16,
        lva_nr_romaneio   TYPE  zsdt0001-nr_romaneio,
        lva_op_tranb_f(1) TYPE c,
        lva_emb_filial(1) TYPE c.


  DATA: lw_xrom_saida       LIKE LINE OF gt_xrom_saida,
        lw_vbak             LIKE LINE OF gt_vbak,
        lt_223_insert       TYPE TABLE OF zlest0223,
        lw_ekko             LIKE LINE OF gt_ekko.

  RANGES: r_matnr_transb FOR zlest0223-produto.

  DATA: lit_zlest0223_ajuste_frete TYPE TABLE OF zlest0223.

  SORT gt_zlest0223_transb BY  id_transporte data_reg.

  CHECK gt_dados_sai_rodo IS NOT INITIAL.

  LOOP AT gt_dados_sai_rodo ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE abfer = 1.

    CLEAR: lw_zlest0223,
           lw_xrom_saida,
           lva_op_tranb_f,
           lva_emb_filial,
           lw_ekko,
           lw_vbak,
           gva_xsaldodistribuir,
           gva_xvlrbrldistribuir,
           gva_xvlrusddistribuir,
           gva_xquantidadevinc,
           lva_id_fiscal_pc,
           lva_id_fiscal_lr,
           lva_id_fiscal_z1,
           lit_zlest0223_ajuste_frete[].

    "Regra AJUSTE_FRETE_0001 - Novas regras para tratamento os cenarios onde estornar a VT e VI para gerar novamente(Correção de preço de frete que ficou errado por exemplo- Frete Terceiro)
    "No cenario abaixo, o codigo identifica se foi cancelado o frete para o fornecimento, e se já foi gerado um novo frete.. Caso tenha um novo frete gerado, o codigo substitui
    "os dados do frete para fornecimento para reeenviar novamente para o sigam.
    SELECT *
      FROM zlest0223 AS a INTO TABLE lit_zlest0223_ajuste_frete
     WHERE fornecimento        = <fs_dados>-vbeln_vl
       AND fornecimento_item   = <fs_dados>-posnr_vl
       AND cancelado           = abap_true
       AND vsart               = <fs_dados>-vsart
       AND shtyp               = <fs_dados>-shtyp
       AND NOT EXISTS ( SELECT fornecimento
                          FROM zlest0223 AS b
                         WHERE b~fornecimento      = a~fornecimento
                           AND b~fornecimento_item = a~fornecimento_item
                           AND b~fknum             = <fs_dados>-vfkp_fknum
                           AND b~vsart             = <fs_dados>-vsart
                           AND b~shtyp             = <fs_dados>-shtyp ).

    IF lit_zlest0223_ajuste_frete[] IS NOT INITIAL.
      SORT lit_zlest0223_ajuste_frete BY id_transporte trecho.

      LOOP AT lit_zlest0223_ajuste_frete ASSIGNING FIELD-SYMBOL(<fs_zlest0223_ajuste_frete>).


        CLEAR: <fs_zlest0223_ajuste_frete>-dt_sincronia_sigam,
               <fs_zlest0223_ajuste_frete>-hr_sincronia_sigam,
               <fs_zlest0223_ajuste_frete>-sincronizado_sigam,

               <fs_zlest0223_ajuste_frete>-dt_cancelamento,
               <fs_zlest0223_ajuste_frete>-hr_cancelamento,
               <fs_zlest0223_ajuste_frete>-cancelado,
               <fs_zlest0223_ajuste_frete>-sld_transb_revertido,

               <fs_zlest0223_ajuste_frete>-dt_atualizacao,
               <fs_zlest0223_ajuste_frete>-hr_atualizacao,
               <fs_zlest0223_ajuste_frete>-us_atualizacao.


        <fs_zlest0223_ajuste_frete>-user_reg             = sy-uname.
        <fs_zlest0223_ajuste_frete>-data_reg             = sy-datum.
        <fs_zlest0223_ajuste_frete>-hora_reg             = sy-uzeit.
        <fs_zlest0223_ajuste_frete>-tknum                = <fs_dados>-tknum.
        <fs_zlest0223_ajuste_frete>-data_transporte      = <fs_dados>-erdat.
        <fs_zlest0223_ajuste_frete>-fknum                = <fs_dados>-vfkp_fknum.
        <fs_zlest0223_ajuste_frete>-data_custo           = <fs_dados>-vfkp_prsdt.
        <fs_zlest0223_ajuste_frete>-tdlnr                = <fs_dados>-lifnr_sp.
        <fs_zlest0223_ajuste_frete>-propr_veiculo        = <fs_dados>-lifnr_pv.
        <fs_zlest0223_ajuste_frete>-vl_brl               = ( <fs_dados>-xbrl / <fs_dados>-vfsi_xqtde_forn ) * <fs_zlest0223_ajuste_frete>-qtde_utilizada.
        <fs_zlest0223_ajuste_frete>-vl_usd               = ( <fs_dados>-xusd / <fs_dados>-vfsi_xqtde_forn ) * <fs_zlest0223_ajuste_frete>-qtde_utilizada.

        <fs_zlest0223_ajuste_frete>-vl_ped_brl           = ( <fs_dados>-vlr_pedagio_brl / <fs_dados>-vfsi_xqtde_forn ) * <fs_zlest0223_ajuste_frete>-qtde_utilizada.
        <fs_zlest0223_ajuste_frete>-vl_ped_usd           = ( <fs_dados>-vlr_pedagio_usd / <fs_dados>-vfsi_xqtde_forn ) * <fs_zlest0223_ajuste_frete>-qtde_utilizada.



        MODIFY zlest0223 FROM <fs_zlest0223_ajuste_frete>.
      ENDLOOP.

      CONTINUE.
    ENDIF.
    "Fim Regra AJUSTE_FRETE_0001

    READ TABLE gt_zlest0223_saida ASSIGNING FIELD-SYMBOL(<fs_zlest0223_saida>) WITH KEY tknum              = <fs_dados>-tknum
                                                                                        fornecimento       = <fs_dados>-vbeln_vl
                                                                                        fornecimento_item  = <fs_dados>-posnr_vl
                                                                                        fknum              = <fs_dados>-vfkp_fknum.
*** Se não existir vamos incluir um registro na tabela ZLEST0223
    CHECK sy-subrc NE 0.

    TRY.
        zcl_fornecedores=>zif_parceiros~get_instance(
           )->set_parceiro( i_parceiro = <fs_dados>-lifnr_pc
           )->ck_parceiro_local_negocio(  IMPORTING e_j_1bbranch = DATA(e_j_1bbranch_for)
           ).
        DATA(_lifnr_pc_filial)  = abap_true.
      CATCH zcx_parceiros.
        _lifnr_pc_filial  = abap_false.
    ENDTRY.

    TRY.
        zcl_clientes=>zif_parceiros~get_instance(
               )->set_parceiro( i_parceiro = <fs_dados>-kunnr_lr
               )->ck_parceiro_local_negocio( IMPORTING e_j_1bbranch = DATA(e_j_1bbranch_cli) ).
        DATA(_kunnr_lr_filial)  = abap_true.
      CATCH zcx_parceiros.
        _kunnr_lr_filial  = abap_false.
    ENDTRY.

    IF <fs_dados>-lifnr_pc IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_pc)
       WHERE lifnr EQ @<fs_dados>-lifnr_pc.

      IF sy-subrc EQ 0.
        IF lwa_lfa1_pc-stcd1 IS NOT INITIAL.
          lva_id_fiscal_pc = lwa_lfa1_pc-stcd1.
        ELSEIF lwa_lfa1_pc-stcd2 IS NOT INITIAL.
          lva_id_fiscal_pc = lwa_lfa1_pc-stcd2.
        ENDIF.
      ENDIF.

    ENDIF.

    IF <fs_dados>-kunnr_lr IS NOT INITIAL.
      SELECT SINGLE *
        FROM kna1 INTO @DATA(lwa_kna1_lr)
       WHERE kunnr EQ @<fs_dados>-kunnr_lr.

      IF sy-subrc EQ 0.
        IF lwa_kna1_lr-stcd1 IS NOT INITIAL.
          lva_id_fiscal_lr = lwa_kna1_lr-stcd1.
        ELSEIF lwa_kna1_lr-stcd2 IS NOT INITIAL.
          lva_id_fiscal_lr = lwa_kna1_lr-stcd2.
        ENDIF.
      ENDIF.

    ENDIF.

    IF <fs_dados>-lifnr_z1 IS NOT INITIAL.
      SELECT SINGLE *
        FROM lfa1 INTO @DATA(lwa_lfa1_z1)
       WHERE lifnr EQ @<fs_dados>-lifnr_z1.

      IF sy-subrc EQ 0.
        IF lwa_lfa1_z1-stcd1 IS NOT INITIAL.
          lva_id_fiscal_z1 = lwa_lfa1_z1-stcd1.
        ELSEIF lwa_lfa1_z1-stcd2 IS NOT INITIAL.
          lva_id_fiscal_z1 = lwa_lfa1_z1-stcd2.
        ENDIF.
      ENDIF.
    ENDIF.

**  Caso for uma ordem de venda
    READ TABLE gt_vbak INTO lw_vbak WITH KEY vbeln = <fs_dados>-vgbel BINARY SEARCH.
    IF sy-subrc = 0.
      IF lw_vbak-auart IN r_auart_rs."

        IF _lifnr_pc_filial = 'X' AND lva_id_fiscal_z1 = lva_id_fiscal_lr.
          lw_zlest0223-tp_trecho  = '03'. "Filial x Porto
        ELSE.
          IF _lifnr_pc_filial = 'X' AND lva_id_fiscal_z1 <> lva_id_fiscal_lr.
            lw_zlest0223-tp_trecho  = '02'. "Filial x Transbordo
            lva_op_tranb_f = 'S'.
          ENDIF.
        ENDIF.
        IF _lifnr_pc_filial <> 'X' AND lva_id_fiscal_z1 = lva_id_fiscal_lr.
          lw_zlest0223-tp_trecho  = '06'. "Fora Filial x Porto
        ELSE.
          IF _lifnr_pc_filial <> 'X' AND lva_id_fiscal_z1 <> lva_id_fiscal_lr.
            lw_zlest0223-tp_trecho  =  '05'.  "Fora Filial x Transbordo
            lva_op_tranb_f = 'S'.
          ENDIF.
        ENDIF.

        IF _lifnr_pc_filial = 'X'.
          lva_emb_filial = 'S'.
        ENDIF.

      ELSE.

        CONTINUE. "Não irá mandar outros tipos de faturamento para o SIGAM. Na primeira fase do projeto, só enviaria ZRFL ZIND ZRDC

        IF _lifnr_pc_filial = 'X'.
          lw_zlest0223-tp_trecho  = '07'. "Filial x Terceiros
          lva_emb_filial = 'S'.
        ELSE.
          IF _lifnr_pc_filial <> 'X'.
            lw_zlest0223-tp_trecho  = '08'. "Fora Filial x Terceiros
          ENDIF.
        ENDIF.
      ENDIF.

    ELSE.
**  Se for um pedido de compra
      READ TABLE gt_ekko INTO lw_ekko WITH KEY ebeln = <fs_dados>-vgbel BINARY SEARCH.
      IF sy-subrc = 0.
        lva_op_tranb_f = 'N'.

        IF _lifnr_pc_filial = 'X'  AND _kunnr_lr_filial = 'X' .
          lw_zlest0223-tp_trecho  = '09'.  "Filial x Filial
        ELSE.
          IF _lifnr_pc_filial <> 'X' AND _kunnr_lr_filial = 'X' ..
            lw_zlest0223-tp_trecho = '01'. "Fora Filial x Filial
          ENDIF.
        ENDIF.

        IF _lifnr_pc_filial = 'X' .
          lva_emb_filial = 'S'.
        ENDIF.

      ENDIF.
    ENDIF.

*---- DADOS PRINCIPAIS.


    READ TABLE gt_xrom_saida INTO lw_xrom_saida WITH KEY doc_rem = <fs_dados>-vbeln_vl.
    IF sy-subrc = 0.
      lw_zlest0223-ch_referencia_rom    =  lw_xrom_saida-ch_referencia.
    ENDIF.

    lw_zlest0223-fornecimento         =  <fs_dados>-vbeln_vl."     ( número do Fornecimento)
    lw_zlest0223-fornecimento_item    =  <fs_dados>-posnr_vl.
    lw_zlest0223-porto_destino        =  <fs_dados>-lifnr_z1.
    lw_zlest0223-local_embarque       =  <fs_dados>-lifnr_pc.
    lw_zlest0223-local_entrega        =  <fs_dados>-kunnr_lr.
    lw_zlest0223-produto              =  <fs_dados>-matnr.
    lw_zlest0223-empresa              =  <fs_dados>-xempresa.
    lw_zlest0223-filial               =  <fs_dados>-xfilial.
    lw_zlest0223-safra                =  <fs_dados>-charg.
    lw_zlest0223-vbtyp_v              =  <fs_dados>-vbtyp.
    lw_zlest0223-tknum                =  <fs_dados>-tknum.
    lw_zlest0223-data_transporte      =  <fs_dados>-erdat.
    lw_zlest0223-fknum                =  <fs_dados>-vfkp_fknum.
    lw_zlest0223-data_custo           =  <fs_dados>-vfkp_prsdt.
    lw_zlest0223-vsart                =  <fs_dados>-vsart.
    lw_zlest0223-tdlnr                =  <fs_dados>-lifnr_sp.
    lw_zlest0223-shtyp                =  <fs_dados>-shtyp.
    lw_zlest0223-propr_veiculo        =  <fs_dados>-lifnr_pv.
    lw_zlest0223-unidade              =  <fs_dados>-gewei.
    lw_zlest0223-nr_ov                =  lw_vbak-vbeln.
    lw_zlest0223-nr_pedido            =  lw_ekko-ebeln.
    lw_zlest0223-transbordo           =  abap_false.
    lw_zlest0223-saldo_transb         =  0 .
    lw_zlest0223-qtde_utilizada       =  0.
    lw_zlest0223-chave_trecho_ref     =  space.
    lw_zlest0223-cancelado            =  abap_false.
    lw_zlest0223-sincronizado_sigam   =  space.
    lw_zlest0223-user_reg             =  sy-uname.
    lw_zlest0223-data_reg             =  sy-datum.
    lw_zlest0223-hora_reg             =  sy-uzeit.

    IF lva_op_tranb_f = 'S'.
      lw_zlest0223-transb_fora_filial = abap_true.
    ELSE.
      lw_zlest0223-transb_fora_filial = abap_false.
    ENDIF.

*** Se possuir um romaneio de entrada vinculado ao romaneio de saída
*** Se ZSDT0001-ID_REFERENCIA estiver preenchida buscar o romaneio de entrada , para localizar o ID_Transporte de entrada

    DATA(lva_achou_rom_ent) = abap_false.
    READ TABLE gt_xrom_entrada ASSIGNING FIELD-SYMBOL(<fs_xrom_entrada>) WITH KEY  branch       = lw_xrom_saida-branch
                                                                                   bukrs        = lw_xrom_saida-bukrs
                                                                                   nr_safra     = lw_xrom_saida-nr_safra
                                                                                   nr_romaneio  = lw_xrom_saida-id_referencia.
    IF sy-subrc = 0.
      READ TABLE gt_zlest0223 INTO DATA(lwa_zlest0223_ent) WITH KEY ch_referencia_rom = <fs_xrom_entrada>-ch_referencia.
      IF sy-subrc EQ 0.
        lva_achou_rom_ent = abap_true.
      ENDIF.
    ENDIF.

    PERFORM f_get_id_transporte CHANGING lw_zlest0223-id_transporte.

    lw_zlest0223-trecho         =   1.
    lw_zlest0223-realizado      =  abap_false.
    lw_zlest0223-qtde_total     =  <fs_dados>-vfsi_xqtde_forn.
    lw_zlest0223-qtde_utilizada =  <fs_dados>-vfsi_xqtde_forn.
    lw_zlest0223-vl_brl         =  <fs_dados>-xbrl.
    lw_zlest0223-vl_usd         =  <fs_dados>-xusd .
    lw_zlest0223-vl_ped_brl     =  <fs_dados>-vlr_pedagio_brl.
    lw_zlest0223-vl_ped_usd     =  <fs_dados>-vlr_pedagio_usd.

    IF lw_zlest0223-transb_fora_filial = abap_true.
      lw_zlest0223-saldo_transb =  lw_zlest0223-qtde_utilizada.
    ELSE.
      lw_zlest0223-saldo_transb =  0.
    ENDIF.

    MODIFY zlest0223 FROM lw_zlest0223.


*    IF lva_achou_rom_ent EQ abap_true.
*
*      lw_zlest0223-id_transporte       = lwa_zlest0223_ent-id_transporte.
*      lw_zlest0223-trecho              = lwa_zlest0223_ent-trecho + 1.
*      lw_zlest0223-realizado           = abap_false.
*      lw_zlest0223-qtde_total          = <fs_dados>-btgew.
*      lw_zlest0223-qtde_utilizada      = <fs_dados>-btgew.
*      lw_zlest0223-vl_brl              = <fs_dados>-xbrl.
*      lw_zlest0223-vl_usd              = <fs_dados>-xusd .
*
*      IF lw_zlest0223-transb_fora_filial = abap_true.
*        lw_zlest0223-saldo_transb =  lw_zlest0223-qtde_utilizada.
*      ELSE.
*        lw_zlest0223-saldo_transb =  0.
*      ENDIF.
*
*      APPEND lw_zlest0223 TO lt_223_insert.
*
*    ELSE.
*
*      IF lva_emb_filial  = 'S'.
*
*        gva_xsaldodistribuir   =  <fs_dados>-vfsi_xqtde_forn.
*        gva_xvlrbrldistribuir  =  <fs_dados>-xbrl.
*        gva_xvlrusddistribuir  =  <fs_dados>-xusd.
*
*        "CLEAR: r_matnr_transb[].
*        "APPEND VALUE #( SIGN = 'I' OPTION = 'EQ' LOW  = <fs_dados>-matnr ) TO r_matnr_transb.
*
*        LOOP AT gt_zlest0223_transb ASSIGNING FIELD-SYMBOL(<fs_zlest0223_transb>) WHERE empresa      = <fs_dados>-xempresa
*                                                                                    AND filial       = <fs_dados>-xfilial
*                                                                                    AND produto      = <fs_dados>-matnr
*                                                                                    AND saldo_transb > 0.
*
*          IF gva_xsaldodistribuir <= 0.
*            EXIT.
*          ENDIF.
*
*          IF gva_xsaldodistribuir > <fs_zlest0223_transb>-saldo_transb.
*            gva_xquantidadevinc  = <fs_zlest0223_transb>-saldo_transb.
*          ELSE.
*            gva_xquantidadevinc  = gva_xsaldodistribuir.
*          ENDIF.
*
*          lw_zlest0223-id_transporte     =  <fs_zlest0223_transb>-id_transporte  .
*          lw_zlest0223-trecho            =  <fs_zlest0223_transb>-trecho + 1.
*          lw_zlest0223-realizado         =  abap_false.
*          lw_zlest0223-qtde_total        =  <fs_dados>-vfsi_xqtde_forn.
*          lw_zlest0223-vl_brl            =  ( <fs_dados>-xbrl / <fs_dados>-vfsi_xqtde_forn ) *  gva_xquantidadevinc.
*          lw_zlest0223-vl_usd            =  ( <fs_dados>-xusd / <fs_dados>-vfsi_xqtde_forn )  * gva_xquantidadevinc.
*          lw_zlest0223-qtde_utilizada    =  gva_xquantidadevinc.
*
*          PERFORM f_monta_chave_trecho_ref USING <fs_zlest0223_transb>
*                                        CHANGING lw_zlest0223-chave_trecho_ref.
*
*          gva_xsaldodistribuir  = gva_xsaldodistribuir  - lw_zlest0223-qtde_utilizada .
*          gva_xvlrbrldistribuir = gva_xvlrbrldistribuir - lw_zlest0223-vl_brl.
*          gva_xvlrusddistribuir = gva_xvlrusddistribuir - lw_zlest0223-vl_usd .
*
*          IF lw_zlest0223-transb_fora_filial EQ abap_true.
*            lw_zlest0223-saldo_transb =  lw_zlest0223-qtde_utilizada.
*          ELSE.
*            lw_zlest0223-saldo_transb =  0.
*          ENDIF.
*
*          PERFORM f_consome_saldo_transbordo USING <fs_zlest0223_transb> gva_xquantidadevinc.
*
*          APPEND lw_zlest0223 TO lt_223_insert.
*        ENDLOOP.
*
*        IF gva_xsaldodistribuir > 0.
*
*          PERFORM f_get_id_transporte CHANGING lw_zlest0223-id_transporte.
*
*          lw_zlest0223-trecho              =  1.
*          lw_zlest0223-realizado           =  abap_false.
*          lw_zlest0223-qtde_total          =  <fs_dados>-vfsi_xqtde_forn.
*          lw_zlest0223-vl_brl              =  gva_xvlrbrldistribuir.
*          lw_zlest0223-vl_usd              =  gva_xvlrusddistribuir.
*          lw_zlest0223-qtde_utilizada      =  gva_xsaldodistribuir.
*          lw_zlest0223-chave_trecho_ref    =  space.
*
*          IF lw_zlest0223-transb_fora_filial EQ abap_true.
*            lw_zlest0223-saldo_transb = lw_zlest0223-qtde_utilizada.
*          ELSE.
*            lw_zlest0223-saldo_transb = 0.
*          ENDIF.
*
*          APPEND lw_zlest0223 TO lt_223_insert.
*
*        ENDIF.
*      ELSE.
*
*        PERFORM f_get_id_transporte CHANGING lw_zlest0223-id_transporte.
*
*        lw_zlest0223-trecho         =   1.
*        lw_zlest0223-realizado      =  abap_false.
*        lw_zlest0223-qtde_total     =  <fs_dados>-vfsi_xqtde_forn.
*        lw_zlest0223-qtde_utilizada =  <fs_dados>-vfsi_xqtde_forn.
*        lw_zlest0223-vl_brl         =  <fs_dados>-xbrl.
*        lw_zlest0223-vl_usd         =  <fs_dados>-xusd .
*
*        IF lw_zlest0223-transb_fora_filial = abap_true.
*          lw_zlest0223-saldo_transb =  lw_zlest0223-qtde_utilizada.
*        ELSE.
*          lw_zlest0223-saldo_transb =  0.
*        ENDIF.
*
*        APPEND lw_zlest0223 TO lt_223_insert.
*
*      ENDIF.
*    ENDIF.

  ENDLOOP.

*  CHECK lt_223_insert[] IS NOT INITIAL.
*  MODIFY zlest0223 FROM TABLE lt_223_insert.

* Comentei Camila Brand - Inicio
*  LOOP AT gt_dados_sai_rodo ASSIGNING FIELD-SYMBOL(<fs_dados>) WHERE abfer = 1.
*
*    " 3.2.1.1. - Busca de romaneio de entrada
*
*    " Via nota fiscal do produtor  (primeira forma de busca)
*    READ TABLE gt_001_sai ASSIGNING FIELD-SYMBOL(<fs_zsd001>)
*      WITH KEY nfnum  = <fs_dados>-znfnum
*               series = <fs_dados>-xserie
*               parid  = <fs_dados>-lifnr_lf
*               branch = <fs_dados>-xfilial
*               nr_safra = <fs_dados>-xsafra.
*
*    IF sy-subrc NE 0.
*
*      " Via nota fiscal própria  ( caso não encontrar o romaneio pela primeira forma de busca)
*      READ TABLE gt_doc_nota_p ASSIGNING FIELD-SYMBOL(<fs_doc_p>)
*        WITH KEY nfenum = <fs_dados>-xnfenum
*                 series = <fs_dados>-xserie
*                 branch = <fs_dados>-xfilial
*                 parid  = <fs_dados>-lifnr_lf.
*
*      IF sy-subrc EQ 0.
*
*        READ TABLE gt_zsdt0232 ASSIGNING FIELD-SYMBOL(<fs_zsdt0232>)
*          WITH KEY docnum = <fs_doc_p>-docnum.
*
*        IF sy-subrc EQ 0.
*
*          READ TABLE gt_001_sai ASSIGNING <fs_zsd001>
*            WITH KEY nfnum  = <fs_zsdt0232>-nnf
*                     series = <fs_zsdt0232>-serie.
*
*        ENDIF.
*
*      ENDIF.
*
*      " Via interface do grãos ( quando não encontrar pela primeira ou segunda forma de busca)
*      READ TABLE gt_zgr_docs ASSIGNING FIELD-SYMBOL(<fs_zdocs>)
*        WITH KEY av_vbeln = <fs_dados>-vbelv.
*
*      IF sy-subrc EQ 0.
*
*        READ TABLE gt_zgr ASSIGNING FIELD-SYMBOL(<fs_zgr>)
*          WITH KEY obj_key = <fs_zdocs>-obj_key.
*
*        IF sy-subrc EQ 0.
*
*          READ TABLE gt_001_sai ASSIGNING <fs_zsd001>
*            WITH KEY ch_referencia =  <fs_zgr>-ch_referencia.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*    " atribuição -----
*
*    IF <fs_zsd001> IS ASSIGNED.
*
*      <fs_dados>-roma_ent_ch_ref = <fs_zsd001>-ch_referencia.
*      <fs_dados>-roma_ent_branch = <fs_zsd001>-branch.
*      <fs_dados>-roma_ent_safra = <fs_zsd001>-nr_safra.
*      <fs_dados>-roma_ent_bukrs = <fs_zsd001>-bukrs.
*
*    ENDIF.
*
*  ENDLOOP.
* Comentei Camila Brand - Fim
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ATUALIZA_DADOS
*&---------------------------------------------------------------------*
FORM f_atualiza_dados .

  DATA lr_datum TYPE RANGE OF sydatum.

  DATA lt_zlest0223 TYPE TABLE OF zlest0223.

  APPEND INITIAL LINE TO lr_datum ASSIGNING FIELD-SYMBOL(<fs_datum>).

  <fs_datum>-sign = 'I'.
  <fs_datum>-option = 'BT'.

  <fs_datum>-low = sy-datum.

  SUBTRACT 30 FROM <fs_datum>-low.

  <fs_datum>-high = sy-datum.

  SELECT * FROM zlest0223
    INTO TABLE lt_zlest0223
      WHERE await_info = 'X'
        AND data_reg IN lr_datum.

  CHECK lt_zlest0223 IS NOT INITIAL.

  " Fazer busca do romaneio de entrada conforme item 3.2.1.1
  SELECT * FROM zsdt0001
    INTO TABLE @DATA(lt_0001_ent)
      FOR ALL ENTRIES IN @lt_zlest0223
        WHERE tp_movimento = 'E'
          AND bukrs = @lt_zlest0223-empresa
          AND branch = @lt_zlest0223-filial
          AND nr_safra = @lt_zlest0223-safra
          AND id_referencia = @lt_zlest0223-ch_referencia_rom.

  " Devemos verificar se o romaneio de entrada esta vinculado a algum romaneio de saída, conforme item 3.2.1.2
  IF sy-subrc EQ 0.

    SELECT * FROM zsdt0001
      INTO TABLE @DATA(lt_0001_sai)
        FOR ALL ENTRIES IN @lt_0001_ent
          WHERE tp_movimento = 'S'
            AND bukrs = @lt_0001_ent-bukrs
            AND branch = @lt_0001_ent-branch
            AND nr_safra = @lt_0001_ent-nr_safra
            AND id_referencia = @lt_0001_ent-ch_referencia.

  ENDIF.

  LOOP AT lt_zlest0223 ASSIGNING FIELD-SYMBOL(<fs_0023>).

    READ TABLE lt_0001_ent ASSIGNING FIELD-SYMBOL(<fs_ent>)
      WITH KEY bukrs = <fs_0023>-empresa
               branch = <fs_0023>-filial
               nr_safra = <fs_0023>-safra
               id_referencia = <fs_0023>-ch_referencia_rom.

    " Se encontrar romaneio de entrada
    IF sy-subrc EQ 0.

      <fs_0023>-await_info  =  ''.
      <fs_0023>-ch_referencia_rom =  <fs_ent>-ch_referencia.

      " Devemos verificar se o romaneio de entrada esta vinculado a algum romaneio de saída, conforme item 3.2.1.2
      READ TABLE lt_0001_sai ASSIGNING FIELD-SYMBOL(<fs_sai>)
        WITH KEY bukrs = <fs_ent>-bukrs
                 branch = <fs_ent>-branch
                 nr_safra = <fs_ent>-nr_safra
                id_referencia = <fs_ent>-ch_referencia.

      " Se encontrar romaneio de saída
      IF sy-subrc EQ 0.
        <fs_0023>-transbordo  = abap_false.
      ELSE.
        <fs_0023>-transbordo  = abap_true.
      ENDIF.

      "Se não encontrar romaneio de entrada
    ELSE.

      <fs_0023>-await_info  =  'X'.
      <fs_0023>-ch_referencia_rom =  space.
      <fs_0023>-transbordo = abap_false.

    ENDIF.

  ENDLOOP.

  CHECK lt_zlest0223 IS NOT INITIAL.

  MODIFY zlest0223 FROM TABLE lt_zlest0223.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_SELEC_DADOS_FERRO_SAI
*&---------------------------------------------------------------------*
FORM f_selec_dados_ferro_sai .

  DATA: lwa_dados_alv LIKE LINE OF gt_dados_alv.

  CHECK gt_dados_alv IS NOT INITIAL.

  LOOP AT gt_dados_alv INTO lwa_dados_alv WHERE abfer = 1
    AND ( vsart = '02' OR  vsart = '07' )  AND vbtyp_v = 'J'.

    APPEND lwa_dados_alv TO gt_dados_ferro.
    CLEAR: lwa_dados_alv.
  ENDLOOP.

  CHECK gt_dados_ferro[] IS NOT INITIAL.

  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223
      FOR ALL ENTRIES IN gt_dados_ferro
        WHERE tknum             = gt_dados_ferro-tknum
          AND fornecimento      = gt_dados_ferro-vbeln_vl
          AND fornecimento_item = gt_dados_ferro-posnr_vl
          AND fknum             = gt_dados_ferro-vfkp_fknum.

  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223_transb
      FOR ALL ENTRIES IN gt_dados_ferro
        WHERE fornecimento        = gt_dados_ferro-vbeln_vl
          AND fornecimento_item   = gt_dados_ferro-posnr_vl
          AND transb_fora_filial  = abap_true
          AND cancelado           <> abap_true   "Diferente de Cancelamento
          AND saldo_transb        > 0 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_DADOS_FERRO_SAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_dados_ferro_sai .

  SORT gt_zlest0223_transb BY  id_transporte data_reg.

  LOOP AT gt_dados_ferro ASSIGNING FIELD-SYMBOL(<fs_dados_ferro>).

    CLEAR:  lw_zlest0223,
            gva_xsaldodistribuir,
            gva_xvlrbrldistribuir,
            gva_xvlrusddistribuir,
            gva_xquantidadevinc.

    READ TABLE  gt_zlest0223 TRANSPORTING NO FIELDS
      WITH KEY  tknum             = <fs_dados_ferro>-tknum
                fornecimento      = <fs_dados_ferro>-vbeln_vl
                fornecimento_item = <fs_dados_ferro>-posnr_vl
                fknum             = <fs_dados_ferro>-vfkp_fknum.

    CHECK sy-subrc NE 0. " Não existe o registro então devemos incluir.

    lw_zlest0223-fornecimento      = <fs_dados_ferro>-vbeln_vl.
    lw_zlest0223-fornecimento_item = <fs_dados_ferro>-posnr_vl.
    lw_zlest0223-tp_trecho         = '04'. " Transbordo x Porto


    READ TABLE gt_vtts ASSIGNING FIELD-SYMBOL(<fs_vtts>) WITH KEY tknum = <fs_dados_ferro>-tknum.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_xrom_saida INTO DATA(lw_xrom_saida) WITH KEY doc_rem = <fs_dados_ferro>-vbeln_vl.
    IF sy-subrc = 0.
      lw_zlest0223-ch_referencia_rom    =  lw_xrom_saida-ch_referencia.
    ENDIF.

    PERFORM f_set_parceiros_from_vtts USING <fs_vtts>
                                   CHANGING lw_zlest0223.

    lw_zlest0223-produto            = <fs_dados_ferro>-matnr.
    lw_zlest0223-empresa            = <fs_dados_ferro>-xempresa.
    lw_zlest0223-filial             = <fs_dados_ferro>-xfilial.
    lw_zlest0223-safra              = <fs_dados_ferro>-charg.
    lw_zlest0223-vbtyp_v            = <fs_dados_ferro>-vbtyp.
    lw_zlest0223-tknum              = <fs_dados_ferro>-tknum.
    lw_zlest0223-data_transporte    = <fs_dados_ferro>-erdat.
    lw_zlest0223-fknum              = <fs_dados_ferro>-vfkp_fknum.
    lw_zlest0223-data_custo         = <fs_dados_ferro>-vfkp_prsdt.
    lw_zlest0223-vsart              = <fs_dados_ferro>-vsart.
    lw_zlest0223-tdlnr              = <fs_dados_ferro>-lifnr_sp.
    lw_zlest0223-shtyp              = <fs_dados_ferro>-shtyp.
    lw_zlest0223-unidade            = <fs_dados_ferro>-gewei.
    lw_zlest0223-propr_veiculo      = space.
    lw_zlest0223-nr_ov              = <fs_dados_ferro>-vgbel.
    lw_zlest0223-nr_pedido          = space.
    lw_zlest0223-transbordo         = abap_false.
    lw_zlest0223-transb_fora_filial = abap_false.
    lw_zlest0223-saldo_transb       = 0.
    lw_zlest0223-qtde_utilizada     = 0.
    lw_zlest0223-chave_trecho_ref   = space.
    lw_zlest0223-cancelado          = abap_false.
    lw_zlest0223-sincronizado_sigam = space.
    lw_zlest0223-user_reg           = sy-uname.
    lw_zlest0223-data_reg           = sy-datum.
    lw_zlest0223-hora_reg           = sy-uzeit.

    DATA(lva_existe_frete_rodo) = abap_false.
    READ TABLE gt_vttp_rodo WITH KEY vbeln = <fs_dados_ferro>-vbeln_vl TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lva_existe_frete_rodo = abap_true.
    ENDIF.

    gva_xsaldodistribuir   =  <fs_dados_ferro>-vfsi_xqtde_forn.
    gva_xvlrbrldistribuir  =  <fs_dados_ferro>-xbrl.
    gva_xvlrusddistribuir  =  <fs_dados_ferro>-xusd.

    LOOP AT gt_zlest0223_transb ASSIGNING FIELD-SYMBOL(<fs_zlest0223_transb>) WHERE fornecimento       = <fs_dados_ferro>-vbeln_vl
                                                                                AND fornecimento_item  = <fs_dados_ferro>-posnr_vl
                                                                                AND saldo_transb       > 0.

      IF gva_xsaldodistribuir <= 0.
        EXIT.
      ENDIF.

      IF gva_xsaldodistribuir > <fs_zlest0223_transb>-saldo_transb.
        gva_xquantidadevinc  = <fs_zlest0223_transb>-saldo_transb.
      ELSE.
        gva_xquantidadevinc  = gva_xsaldodistribuir.
      ENDIF.

      lw_zlest0223-id_transporte     =  <fs_zlest0223_transb>-id_transporte  .
      lw_zlest0223-trecho            =  <fs_zlest0223_transb>-trecho + 1.
      lw_zlest0223-realizado         =  abap_false.
      lw_zlest0223-qtde_total        =  <fs_dados_ferro>-vfsi_xqtde_forn.
      lw_zlest0223-vl_brl            =  ( <fs_dados_ferro>-xbrl / <fs_dados_ferro>-vfsi_xqtde_forn )  *  gva_xquantidadevinc.
      lw_zlest0223-vl_usd            =  ( <fs_dados_ferro>-xusd / <fs_dados_ferro>-vfsi_xqtde_forn )  *  gva_xquantidadevinc.
      lw_zlest0223-qtde_utilizada    =  gva_xquantidadevinc.

      PERFORM f_monta_chave_trecho_ref USING <fs_zlest0223_transb>
                                    CHANGING lw_zlest0223-chave_trecho_ref.

      gva_xsaldodistribuir  = gva_xsaldodistribuir  - lw_zlest0223-qtde_utilizada.
      gva_xvlrbrldistribuir = gva_xvlrbrldistribuir - lw_zlest0223-vl_brl.
      gva_xvlrusddistribuir = gva_xvlrusddistribuir - lw_zlest0223-vl_usd .

      PERFORM f_consome_saldo_transbordo USING <fs_zlest0223_transb> gva_xquantidadevinc.

      MODIFY zlest0223 FROM lw_zlest0223.

    ENDLOOP.

    IF ( gva_xsaldodistribuir > 0 ). "AND ( lva_existe_frete_rodo = abap_false ). "Não pode ter gerar um "Id Transporte" novo se possuir frete rodoviario

      SELECT SINGLE *
        from zlest0223 INTO @DATA(lwa_frete_saida_exists)
       WHERE trecho EQ 1
         and tknum  EQ @lw_zlest0223-tknum
         and fknum  EQ @lw_zlest0223-fknum.

      IF sy-subrc eq 0.
        lw_zlest0223-id_transporte = lwa_frete_saida_exists-id_transporte.
      ELSE.
        PERFORM f_get_id_transporte CHANGING lw_zlest0223-id_transporte.
      ENDIF.

      lw_zlest0223-trecho              =  1.
      lw_zlest0223-realizado           =  abap_false.
      lw_zlest0223-qtde_total          =  <fs_dados_ferro>-vfsi_xqtde_forn.
      lw_zlest0223-vl_brl              =  gva_xvlrbrldistribuir.
      lw_zlest0223-vl_usd              =  gva_xvlrusddistribuir.
      lw_zlest0223-qtde_utilizada      =  gva_xsaldodistribuir.
      lw_zlest0223-chave_trecho_ref    =  space.
      lw_zlest0223-saldo_transb        =  0.

      MODIFY zlest0223 FROM lw_zlest0223.
    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_selec_dados_aqua_sai .

  DATA: lwa_dados_alv LIKE LINE OF gt_dados_alv.

  CHECK gt_dados_alv IS NOT INITIAL.

  LOOP AT gt_dados_alv INTO lwa_dados_alv WHERE abfer   = 1
                                            AND vsart   = '03'
                                            AND vbtyp_v = 'J'.

    APPEND lwa_dados_alv TO gt_dados_aqua.
    CLEAR: lwa_dados_alv.
  ENDLOOP.

  CHECK gt_dados_aqua[] IS NOT INITIAL.

  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223
      FOR ALL ENTRIES IN gt_dados_aqua
        WHERE tknum             = gt_dados_aqua-tknum
          AND fornecimento      = gt_dados_aqua-vbeln_vl
          AND fornecimento_item = gt_dados_aqua-posnr_vl
          AND fknum             = gt_dados_aqua-vfkp_fknum.

  SELECT * FROM zlest0223
    INTO TABLE gt_zlest0223_transb
      FOR ALL ENTRIES IN gt_dados_aqua
        WHERE fornecimento            = gt_dados_aqua-vbeln_vl
          AND fornecimento_item       = gt_dados_aqua-posnr_vl
          AND transb_fora_filial      = abap_true
          AND cancelado               <> abap_true   "Diferente de Cancelamento
          AND saldo_transb            > 0 .

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_DADOS_FERRO_SAI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_grava_dados_aqua_sai .

  SORT gt_zlest0223_transb BY  id_transporte data_reg.

  LOOP AT gt_dados_aqua ASSIGNING FIELD-SYMBOL(<fs_dados_aqua>).

    CLEAR:  lw_zlest0223,
            gva_xsaldodistribuir,
            gva_xvlrbrldistribuir,
            gva_xvlrusddistribuir,
            gva_xquantidadevinc.

    READ TABLE  gt_zlest0223 TRANSPORTING NO FIELDS
      WITH KEY  tknum               = <fs_dados_aqua>-tknum
                fornecimento        = <fs_dados_aqua>-vbeln_vl
                fornecimento_item   = <fs_dados_aqua>-posnr_vl
                fknum               = <fs_dados_aqua>-vfkp_fknum.

    CHECK sy-subrc NE 0. " Não existe o registro então devemos incluir.

    lw_zlest0223-fornecimento        = <fs_dados_aqua>-vbeln_vl.
    lw_zlest0223-fornecimento_item   = <fs_dados_aqua>-posnr_vl.
    lw_zlest0223-tp_trecho           = '04'. " Transbordo x Porto


    READ TABLE gt_vtts ASSIGNING FIELD-SYMBOL(<fs_vtts>) WITH KEY tknum = <fs_dados_aqua>-tknum.
    CHECK sy-subrc EQ 0.

    READ TABLE gt_xrom_saida INTO DATA(lw_xrom_saida) WITH KEY doc_rem = <fs_dados_aqua>-vbeln_vl.
    IF sy-subrc = 0.
      lw_zlest0223-ch_referencia_rom    =  lw_xrom_saida-ch_referencia.
    ENDIF.

    PERFORM f_set_parceiros_from_vtts USING <fs_vtts>
                                   CHANGING lw_zlest0223.

    lw_zlest0223-produto            = <fs_dados_aqua>-matnr.
    lw_zlest0223-empresa            = <fs_dados_aqua>-xempresa.
    lw_zlest0223-filial             = <fs_dados_aqua>-xfilial.
    lw_zlest0223-safra              = <fs_dados_aqua>-charg.
    lw_zlest0223-vbtyp_v            = <fs_dados_aqua>-vbtyp.
    lw_zlest0223-tknum              = <fs_dados_aqua>-tknum.
    lw_zlest0223-data_transporte    = <fs_dados_aqua>-erdat.
    lw_zlest0223-fknum              = <fs_dados_aqua>-vfkp_fknum.
    lw_zlest0223-data_custo         = <fs_dados_aqua>-vfkp_prsdt.
    lw_zlest0223-vsart              = <fs_dados_aqua>-vsart.
    lw_zlest0223-tdlnr              = <fs_dados_aqua>-lifnr_sp.
    lw_zlest0223-shtyp              = <fs_dados_aqua>-shtyp.
    lw_zlest0223-unidade            = <fs_dados_aqua>-gewei.
    lw_zlest0223-propr_veiculo      = space.
    lw_zlest0223-nr_ov              = <fs_dados_aqua>-vgbel.
    lw_zlest0223-nr_pedido          = space.
    lw_zlest0223-transbordo         = abap_false.
    lw_zlest0223-transb_fora_filial = abap_false.
    lw_zlest0223-saldo_transb       = 0.
    lw_zlest0223-qtde_utilizada     = 0.
    lw_zlest0223-chave_trecho_ref   = space.
    lw_zlest0223-cancelado          = abap_false.
    lw_zlest0223-sincronizado_sigam = space.
    lw_zlest0223-user_reg           = sy-uname.
    lw_zlest0223-data_reg           = sy-datum.
    lw_zlest0223-hora_reg           = sy-uzeit.

    DATA(lva_existe_frete_rodo) = abap_false.
    READ TABLE gt_vttp_rodo WITH KEY vbeln = <fs_dados_aqua>-vbeln_vl TRANSPORTING NO FIELDS.
    IF sy-subrc EQ 0.
      lva_existe_frete_rodo = abap_true.
    ENDIF.

    gva_xsaldodistribuir   =  <fs_dados_aqua>-vfsi_xqtde_forn.
    gva_xvlrbrldistribuir  =  <fs_dados_aqua>-xbrl.
    gva_xvlrusddistribuir  =  <fs_dados_aqua>-xusd.

    LOOP AT gt_zlest0223_transb ASSIGNING FIELD-SYMBOL(<fs_zlest0223_transb>) WHERE fornecimento      = <fs_dados_aqua>-vbeln_vl
                                                                                AND fornecimento_item = <fs_dados_aqua>-posnr_vl
                                                                                AND saldo_transb      > 0.

      IF gva_xsaldodistribuir <= 0.
        EXIT.
      ENDIF.

      IF gva_xsaldodistribuir > <fs_zlest0223_transb>-saldo_transb.
        gva_xquantidadevinc  = <fs_zlest0223_transb>-saldo_transb.
      ELSE.
        gva_xquantidadevinc  = gva_xsaldodistribuir.
      ENDIF.

      lw_zlest0223-id_transporte     =  <fs_zlest0223_transb>-id_transporte  .
      lw_zlest0223-trecho            =  <fs_zlest0223_transb>-trecho + 1.
      lw_zlest0223-realizado         =  abap_false.
      lw_zlest0223-qtde_total        =  <fs_dados_aqua>-vfsi_xqtde_forn.
      lw_zlest0223-vl_brl            =  ( <fs_dados_aqua>-xbrl / <fs_dados_aqua>-vfsi_xqtde_forn )  *  gva_xquantidadevinc.
      lw_zlest0223-vl_usd            =  ( <fs_dados_aqua>-xusd / <fs_dados_aqua>-vfsi_xqtde_forn )  *  gva_xquantidadevinc.
      lw_zlest0223-qtde_utilizada    =  gva_xquantidadevinc.

      PERFORM f_monta_chave_trecho_ref USING <fs_zlest0223_transb>
                                    CHANGING lw_zlest0223-chave_trecho_ref.

      gva_xsaldodistribuir  = gva_xsaldodistribuir  - lw_zlest0223-qtde_utilizada.
      gva_xvlrbrldistribuir = gva_xvlrbrldistribuir - lw_zlest0223-vl_brl.
      gva_xvlrusddistribuir = gva_xvlrusddistribuir - lw_zlest0223-vl_usd .

      PERFORM f_consome_saldo_transbordo USING <fs_zlest0223_transb> gva_xquantidadevinc.

      MODIFY zlest0223 FROM lw_zlest0223.

    ENDLOOP.

    IF ( gva_xsaldodistribuir > 0 ). "AND ( lva_existe_frete_rodo = abap_false ). "Não pode ter gerar um "Id Transporte" novo se possuir frete rodoviario

      SELECT SINGLE *
        from zlest0223 INTO @DATA(lwa_frete_saida_exists)
       WHERE trecho EQ 1
         and tknum  EQ @lw_zlest0223-tknum
         and fknum  EQ @lw_zlest0223-fknum.

      IF sy-subrc eq 0.
        lw_zlest0223-id_transporte = lwa_frete_saida_exists-id_transporte.
      ELSE.
        PERFORM f_get_id_transporte CHANGING lw_zlest0223-id_transporte.
      ENDIF.

      lw_zlest0223-trecho              =  1.
      lw_zlest0223-realizado           =  abap_false.
      lw_zlest0223-qtde_total          =  <fs_dados_aqua>-vfsi_xqtde_forn.
      lw_zlest0223-vl_brl              =  gva_xvlrbrldistribuir.
      lw_zlest0223-vl_usd              =  gva_xvlrusddistribuir.
      lw_zlest0223-qtde_utilizada      =  gva_xsaldodistribuir.
      lw_zlest0223-chave_trecho_ref    =  space.
      lw_zlest0223-saldo_transb        =  0.

      MODIFY zlest0223 FROM lw_zlest0223.

    ENDIF.
  ENDLOOP.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  F_SELEC_SIGAM
*&---------------------------------------------------------------------*
FORM f_selec_alteracao_canc_transp.

  DATA lv_data TYPE sy-datum.

  " caso nao esteja preenchido....
  IF gv_dias_busca IS INITIAL.

    gv_dias_busca = 5.

  ENDIF.

  lv_data = sy-datum.

  SUBTRACT gv_dias_busca FROM lv_data.

  " --------------------- 2.1 Seleção Cartas de Correção

  SELECT * FROM zcarta_correcao
    INTO TABLE gt_zcarta
      WHERE dt_authcod >= lv_data.

  DELETE gt_zcarta WHERE authcode IS INITIAL.
  DELETE gt_zcarta WHERE ( novo_terminal    IS INITIAL ) AND
                         ( novo_agente      IS INITIAL ) AND
                         ( novo_loc_coleta  IS INITIAL ) AND
                         ( novo_loc_entrega IS INITIAL ).

  IF gt_zcarta[] IS NOT INITIAL.

    SORT gt_zcarta BY docnum dt_authcod hr_authcod DESCENDING.

    " --------------------- 2.2. Seleção Documentos Fiscais da Carta Correção

    SELECT j_1bnfdoc~docnum refkey reftyp FROM j_1bnfdoc
      INNER JOIN j_1bnflin ON j_1bnfdoc~docnum = j_1bnflin~docnum
        INTO CORRESPONDING FIELDS OF TABLE gt_doc_vbfa
          FOR ALL ENTRIES IN gt_zcarta
            WHERE j_1bnfdoc~docnum = gt_zcarta-docnum.

    DELETE gt_doc_vbfa WHERE reftyp NE 'MD' AND reftyp NE 'BI'.
    DELETE gt_doc_vbfa WHERE refkey IS INITIAL.

    LOOP AT gt_doc_vbfa ASSIGNING FIELD-SYMBOL(<fs_doc>).
      <fs_doc>-vbeln = <fs_doc>-refkey(10).
    ENDLOOP.

    IF gt_doc_vbfa[] IS NOT INITIAL.

      " ---------------------  2.3 Seleção Documentos de Fornecimento
      SELECT vbeln,vbelv FROM vbfa
        INTO TABLE @DATA(lt_vbfa)
          FOR ALL ENTRIES IN @gt_doc_vbfa
            WHERE vbeln = @gt_doc_vbfa-vbeln
              AND vbtyp_n IN ('R','M')
              AND vbtyp_v IN ('J','7').

      LOOP AT lt_vbfa ASSIGNING FIELD-SYMBOL(<fs_vbfa>).

        READ TABLE gt_doc_vbfa ASSIGNING <fs_doc>
          WITH KEY vbeln = <fs_vbfa>-vbeln.

        CHECK sy-subrc EQ 0.

        <fs_doc>-vbelv = <fs_vbfa>-vbelv.
      ENDLOOP.

      " ---------------------  2.4 Seleção Trechos de Transporte
      SELECT * FROM zlest0223
        INTO TABLE gt_0223_alt_cce
          FOR ALL ENTRIES IN gt_doc_vbfa
            WHERE fornecimento = gt_doc_vbfa-vbelv.
    ENDIF.

  ENDIF.

  DATA lr_trans TYPE RANGE OF tknum.
  DATA lr_liefe TYPE RANGE OF vbeln.
  "DATA lt_alt_frete TYPE TABLE OF ty_frete_ref.

  " ---------------------  2.5 - Seleção de Fretes Alterados/Excluidos
  SELECT * FROM cdhdr
    INTO CORRESPONDING FIELDS OF TABLE gt_cdhdr
    WHERE udate >= lv_data.

  DELETE gt_cdhdr WHERE objectclas NE 'TRANSPORT' AND objectclas NE 'LIEFERUNG'.

  LOOP AT gt_cdhdr ASSIGNING FIELD-SYMBOL(<fs_cdhdr>).
    IF <fs_cdhdr>-objectclas = 'TRANSPORT'.
      <fs_cdhdr>-refkey_vt = <fs_cdhdr>-objectid(10).
    ELSEIF <fs_cdhdr>-objectclas = 'LIEFERUNG'.
      <fs_cdhdr>-refkey_vl = <fs_cdhdr>-objectid(10).
    ENDIF.
  ENDLOOP.

  DATA(gt_cdhdr_vt) = gt_cdhdr[].
  DATA(gt_cdhdr_vl) = gt_cdhdr[].

  DELETE gt_cdhdr_vt WHERE refkey_vt IS INITIAL.
  DELETE gt_cdhdr_vl WHERE refkey_vl IS INITIAL.


  IF gt_cdhdr_vt[] IS NOT INITIAL.

    SELECT * FROM zlest0223
    INTO CORRESPONDING FIELDS OF TABLE gt_0223_alt_frete
      FOR ALL ENTRIES IN gt_cdhdr_vt
        WHERE tknum = gt_cdhdr_vt-refkey_vt.

  ENDIF.

  IF gt_cdhdr_vl[] IS NOT INITIAL.

    SELECT * FROM zlest0223
    APPENDING CORRESPONDING FIELDS OF TABLE gt_0223_alt_frete
      FOR ALL ENTRIES IN gt_cdhdr_vl
        WHERE fornecimento = gt_cdhdr_vl-refkey_vl.

  ENDIF.

  DELETE gt_0223_alt_frete
    WHERE cancelado = 'X'
      AND sld_transb_revertido  = 'X'
      AND chave_trecho_ref IS NOT INITIAL.

  DELETE gt_0223_alt_frete
    WHERE cancelado = 'X'
      AND chave_trecho_ref IS INITIAL.

  SORT gt_0223_alt_frete BY id_transporte trecho fornecimento fornecimento_item fknum .
  DELETE ADJACENT DUPLICATES FROM gt_0223_alt_frete COMPARING id_transporte trecho fornecimento fornecimento_item fknum.

  LOOP AT gt_0223_alt_frete ASSIGNING FIELD-SYMBOL(<fs_alt_frete>) WHERE chave_trecho_ref IS NOT INITIAL.

    <fs_alt_frete>-ref_id_transporte      = <fs_alt_frete>-chave_trecho_ref(10).
    <fs_alt_frete>-ref_trecho             = <fs_alt_frete>-chave_trecho_ref+11(1).
    <fs_alt_frete>-ref_fornecimento       = <fs_alt_frete>-chave_trecho_ref+13(10).
    <fs_alt_frete>-ref_fornecimento_item  = <fs_alt_frete>-chave_trecho_ref+24(06).
    <fs_alt_frete>-ref_fknum              = <fs_alt_frete>-chave_trecho_ref+31(10).

  ENDLOOP.

  IF gt_0223_alt_frete[] IS NOT INITIAL.

    SELECT * FROM vfkk
     INTO TABLE gt_vfkk_sigam
      FOR ALL ENTRIES IN gt_0223_alt_frete
        WHERE fknum = gt_0223_alt_frete-fknum.

    SELECT * FROM likp
        INTO TABLE gt_likp_sigam
          FOR ALL ENTRIES IN gt_0223_alt_frete
          WHERE vbeln EQ gt_0223_alt_frete-fornecimento.

    SELECT * FROM vttk
        INTO TABLE gt_vttk_sigam
        FOR ALL ENTRIES IN gt_0223_alt_frete
          WHERE tknum EQ gt_0223_alt_frete-tknum.

  ENDIF.


  DATA(gt_0223_alt_frete_aux) = gt_0223_alt_frete[].
  DELETE gt_0223_alt_frete_aux WHERE chave_trecho_ref IS INITIAL.

  IF gt_0223_alt_frete_aux[] IS NOT INITIAL.

    SELECT * FROM zlest0223
      INTO TABLE gt_0223_frt_ref
      FOR ALL ENTRIES IN gt_0223_alt_frete_aux
        WHERE id_transporte      = gt_0223_alt_frete_aux-ref_id_transporte
          AND trecho             = gt_0223_alt_frete_aux-ref_trecho
          AND fornecimento       = gt_0223_alt_frete_aux-ref_fornecimento
          AND fornecimento_item  = gt_0223_alt_frete_aux-ref_fornecimento_item
          AND fknum              = gt_0223_alt_frete_aux-ref_fknum.

  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_PROCE_SIGAM
*&---------------------------------------------------------------------*
FORM f_proce_alteracao_canc_transp.

  DATA(lv_frete_cancel) = space.
  DATA(lv_revrt_saldo) = space.

  DATA: lva_zlest0223_gravar TYPE zlest0223.

  DATA: lva_qtde_total_transb TYPE zlest0223-qtde_total,
        lva_qtde_vinc_transb  TYPE zlest0223-qtde_total.

  DATA: lit_zlest0223_ref TYPE TABLE OF zlest0223.

  " 3.1 - Ajuste de Parceiros alterados via carta de correção

  LOOP AT gt_0223_alt_cce ASSIGNING FIELD-SYMBOL(<fs_alt_cce>).

    DATA(lva_alterou) = abap_false.

    READ TABLE gt_doc_vbfa ASSIGNING FIELD-SYMBOL(<fs_doc_vbfa>)
      WITH KEY vbelv = <fs_alt_cce>-fornecimento.

    CHECK sy-subrc EQ 0.

    READ TABLE gt_zcarta ASSIGNING FIELD-SYMBOL(<fs_zcarta>)
      WITH KEY docnum = <fs_doc_vbfa>-docnum.

    CHECK sy-subrc EQ 0.

    IF ( <fs_zcarta>-novo_terminal IS NOT INITIAL ) AND ( <fs_zcarta>-novo_terminal NE <fs_alt_cce>-porto_destino ).
      <fs_alt_cce>-porto_destino = <fs_zcarta>-novo_terminal.
      lva_alterou = abap_true.
    ENDIF.

    IF ( <fs_zcarta>-novo_agente IS NOT INITIAL ) AND ( <fs_zcarta>-novo_agente NE  <fs_alt_cce>-tdlnr ).
      <fs_alt_cce>-tdlnr = <fs_zcarta>-novo_agente.
      lva_alterou = abap_true.
    ENDIF.

    IF ( <fs_zcarta>-novo_loc_coleta IS NOT INITIAL ) AND ( <fs_zcarta>-novo_loc_coleta NE <fs_alt_cce>-local_embarque ).
      <fs_alt_cce>-local_embarque = <fs_zcarta>-novo_loc_coleta.
      lva_alterou = abap_true.
    ENDIF.

    IF ( <fs_zcarta>-novo_loc_entrega IS NOT INITIAL ) AND ( <fs_zcarta>-novo_loc_entrega NE <fs_alt_cce>-local_entrega ).
      <fs_alt_cce>-local_entrega  = <fs_zcarta>-novo_loc_entrega.
      lva_alterou = abap_true.
    ENDIF.

    IF lva_alterou = abap_true.

      IF NOT ( ( <fs_alt_cce>-cancelado EQ abap_true ) AND ( <fs_alt_cce>-sincronizado_sigam = abap_true ) ).
        <fs_alt_cce>-sincronizado_sigam  = space.
      ENDIF.

      <fs_alt_cce>-dt_atualizacao      = sy-datum.
      <fs_alt_cce>-hr_atualizacao      = sy-uzeit.
      <fs_alt_cce>-us_atualizacao      = sy-uname.

      MODIFY zlest0223 FROM <fs_alt_cce>.

    ENDIF.

  ENDLOOP.

  " 3.2 - Identificação de alterações/exclusão de fretes
  LOOP AT gt_0223_alt_frete ASSIGNING FIELD-SYMBOL(<fs_alt_frete>).

    DATA(lv_tabix) = sy-tabix.

    lv_frete_cancel = space.
    lv_revrt_saldo = space.

    IF <fs_alt_frete>-transbordo = 'X'
     AND <fs_alt_frete>-trecho = '1'
     AND <fs_alt_frete>-tp_trecho = '01'.

      SELECT COUNT(*) FROM zlest0223
         WHERE id_transporte  = <fs_alt_frete>-id_transporte
           AND trecho <> '1'.

      " Se encontrar registros, não devemos seguir com os procedimentos abaixo.
      " Devemos seguir com o processamento do proximo registro -
      " Essa reversão será tratada em outra US, no pacote dos 20%
      IF sy-dbcnt > 0.
        "DELETE gt_0223_alt_frete INDEX lv_tabix.
        CONTINUE.
      ENDIF.

    ENDIF.

    IF <fs_alt_frete>-cancelado <> 'X'.

      READ TABLE gt_vfkk_sigam ASSIGNING FIELD-SYMBOL(<fs_vfkk>) WITH KEY fknum = <fs_alt_frete>-fknum.

      IF sy-subrc NE 0 .
        lv_frete_cancel = 'X'.
      ENDIF.

      READ TABLE gt_vttk_sigam ASSIGNING FIELD-SYMBOL(<fs_vttk>) WITH KEY tknum = <fs_alt_frete>-tknum.

      IF sy-subrc NE 0 .
        lv_frete_cancel = 'X'.
      ENDIF.

      IF lv_frete_cancel = 'X'.

        <fs_alt_frete>-sincronizado_sigam  = space.
        <fs_alt_frete>-cancelado           = 'X'.
        <fs_alt_frete>-dt_cancelamento     = sy-datum.
        <fs_alt_frete>-hr_cancelamento     = sy-uzeit.
        <fs_alt_frete>-dt_atualizacao      = sy-datum.
        <fs_alt_frete>-hr_atualizacao      = sy-uzeit.
        <fs_alt_frete>-us_atualizacao      = sy-uname.

        CLEAR:lva_zlest0223_gravar.
        MOVE-CORRESPONDING <fs_alt_frete> TO lva_zlest0223_gravar.

        MODIFY zlest0223 FROM lva_zlest0223_gravar.
        COMMIT WORK AND WAIT.

      ENDIF.

    ENDIF.

    IF ( lv_frete_cancel = 'X' ) OR ( <fs_alt_frete>-cancelado = 'X' ) AND
       ( <fs_alt_frete>-vsart = '02' OR  "Ferroviario
         <fs_alt_frete>-vsart = '07' OR  "Ferroviario
         <fs_alt_frete>-vsart = '03' ).  "Aquaviario
      lv_revrt_saldo = 'X'.
    ENDIF.

    IF <fs_alt_frete>-chave_trecho_ref IS NOT INITIAL AND
       <fs_alt_frete>-cancelado = 'X'  AND
       <fs_alt_frete>-sld_transb_revertido <> 'X'.

      READ TABLE gt_likp_sigam ASSIGNING FIELD-SYMBOL(<fs_likp>)
              WITH KEY vbeln = <fs_alt_frete>-fornecimento.

      IF sy-subrc NE 0.

        SELECT COUNT(*) FROM zlest0223
          WHERE id_transporte        = <fs_alt_frete>-id_transporte
            AND trecho               = <fs_alt_frete>-trecho
            AND fornecimento         = <fs_alt_frete>-fornecimento
            AND fornecimento_item    = <fs_alt_frete>-fornecimento_item
            AND sld_transb_revertido = 'X'.

        IF sy-dbcnt = 0.
          lv_revrt_saldo = 'X'.
        ENDIF.
      ENDIF.

    ENDIF.

    IF lv_revrt_saldo = 'X'.

      READ TABLE gt_0223_frt_ref ASSIGNING FIELD-SYMBOL(<fs_frt_ref>)
          WITH KEY id_transporte      = <fs_alt_frete>-ref_id_transporte
                   trecho             = <fs_alt_frete>-ref_trecho
                   fornecimento       = <fs_alt_frete>-ref_fornecimento
                   fornecimento_item  = <fs_alt_frete>-ref_fornecimento_item
                   fknum              = <fs_alt_frete>-ref_fknum.

      IF sy-subrc EQ 0.

        "<fs_frt_ref>-saldo_transb = <fs_frt_ref>-saldo_transb + <fs_alt_frete>-quant_vinc.

        CLEAR:lit_zlest0223_ref[], lva_qtde_total_transb, lva_qtde_vinc_transb.

        lva_qtde_total_transb = <fs_frt_ref>-qtde_utilizada.

        SELECT *
          FROM zlest0223 INTO TABLE lit_zlest0223_ref
         WHERE chave_trecho_ref = <fs_alt_frete>-chave_trecho_ref
           AND cancelado        = abap_false.

        LOOP AT lit_zlest0223_ref INTO DATA(lwa_frete_ref).
          ADD lwa_frete_ref-qtde_utilizada TO lva_qtde_vinc_transb.
        ENDLOOP.

        <fs_frt_ref>-saldo_transb = lva_qtde_total_transb - lva_qtde_vinc_transb.

        MODIFY zlest0223 FROM <fs_frt_ref>.
        COMMIT WORK AND WAIT.

        <fs_alt_frete>-sld_transb_revertido = 'X'.

        CLEAR:lva_zlest0223_gravar.
        MOVE-CORRESPONDING <fs_alt_frete> TO lva_zlest0223_gravar.

        MODIFY zlest0223 FROM lva_zlest0223_gravar.
        COMMIT WORK AND WAIT.
      ENDIF.
    ENDIF.


  ENDLOOP.



ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_ENVIO_SIGAM_FRETE_REALIZADO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_envio_sigam_frete_realizado .



  DATA: lva_max_reg_send     TYPE i.
  DATA: lva_current_metodo   TYPE C LENGTH 50.
  DATA: lva_registro_metodo  TYPE C LENGTH 50.
  DATA: lva_current_id_group TYPE i.



  DATA: lit_zlest0223_env_sigam TYPE TABLE OF ty_zlest0223.
  DATA: lva_index       TYPE sy-tabix.

  DATA: lit_dados_send  TYPE TABLE OF ty_zlest0223.

  DATA: lwa_dados       TYPE ty_dados.
  DATA: lit_dados_post  TYPE TABLE OF ty_dados.

  DATA: lwa_dados_canc  TYPE ty_dados_canc.
  DATA: lit_dados_canc  TYPE TABLE OF ty_dados_canc.

  DATA: lva_return TYPE string.

  SELECT SINGLE *
    from tvarvc INTO @DATA(lwa_tvarvx_max_reg)
   WHERE name eq 'ZSDR0129_MAX_REG_SEND'.

  IF sy-subrc eq 0.
    lva_max_reg_send = lwa_tvarvx_max_reg-low.
  ENDIF.

  IF lva_max_reg_send eq 0.
    lva_max_reg_send = 1.
  ENDIF.

  SELECT * FROM zlest0223
  INTO TABLE lit_zlest0223_env_sigam
    WHERE sincronizado_sigam = ''
      AND await_info = ''.

  SORT lit_zlest0223_env_sigam BY  data_reg hora_reg dt_atualizacao hr_atualizacao .

*  LOOP AT lit_zlest0223_env_sigam INTO DATA(lwa_zlest0223_env_sigam).
*    lva_index = sy-tabix.
*
*    lwa_zlest0223_env_sigam-xqtde_env_sigam = lwa_zlest0223_env_sigam-qtde_utilizada.
*    MODIFY lit_zlest0223_env_sigam FROM lwa_zlest0223_env_sigam INDEX lva_index.
*
*  ENDLOOP.


  CHECK lit_zlest0223_env_sigam[] IS NOT INITIAL.

  SELECT *
    FROM vbak INTO TABLE @DATA(lit_vbak)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE vbeln EQ @lit_zlest0223_env_sigam-nr_ov.

  SELECT *
    FROM ekko INTO TABLE @DATA(lit_ekko)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE ebeln EQ @lit_zlest0223_env_sigam-nr_pedido.

  SELECT *
    FROM zlest0108 INTO TABLE @DATA(lit_zlest0108)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE vbeln EQ @lit_zlest0223_env_sigam-fornecimento.

  SELECT *
    FROM zsdt0001 INTO TABLE @DATA(lit_zsdt0001)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE ch_referencia EQ @lit_zlest0223_env_sigam-ch_referencia_rom.

  LOOP AT lit_zlest0223_env_sigam ASSIGNING FIELD-SYMBOL(<fs_zlest0223_upd>).
    <fs_zlest0223_upd>-tp_trecho_v  =  <fs_zlest0223_upd>-tp_trecho.
    <fs_zlest0223_upd>-tp_forn_v    =  <fs_zlest0223_upd>-vbtyp_v.
  ENDLOOP.

  SELECT *
    FROM DD07T INTO TABLE @DATA(lit_DD07T_TP_TRECHO)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE DOMNAME    EQ 'ZDD_TP_TRECHO'
     and DOMVALUE_L EQ  @lit_zlest0223_env_sigam-tp_trecho_v
     AND ddlanguage eq @sy-langu.

  SELECT *
    FROM DD07T INTO TABLE @DATA(lit_DD07T_TP_FORN)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE DOMNAME    EQ 'VBTYP'
     and DOMVALUE_L EQ @lit_zlest0223_env_sigam-tp_forn_v
     AND ddlanguage eq @sy-langu.

  SELECT *
    FROM T173T INTO TABLE @DATA(lit_T173T)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE SPRAS    EQ @SY-LANGU
     and VSART    EQ @lit_zlest0223_env_sigam-vsart.

  SELECT *
    FROM TVTKT INTO TABLE @DATA(lit_TVTKT)
    FOR ALL ENTRIES IN @lit_zlest0223_env_sigam
   WHERE SPRAS EQ @SY-LANGU
     and SHTYP EQ @lit_zlest0223_env_sigam-SHTYP.

  SORT: lit_vbak      BY vbeln,
        lit_ekko      BY ebeln,
        lit_zlest0108 BY vbeln,
        lit_zsdt0001  BY ch_referencia,
        lit_dd07t_tp_trecho by domvalue_l,
        lit_dd07t_tp_forn   by domvalue_l,
        lit_t173t     by vsart,
        lit_tvtkt     by shtyp.

  DATA(lva_frete_sem_inf_rom) = abap_false.

  CLEAR: lit_group_send[].

  LOOP AT lit_zlest0223_env_sigam INTO DATA(lwa_zlest0223_env_sigam).

    DATA(_lva_tabix) = sy-tabix.

    IF lwa_zlest0223_env_sigam-ch_referencia_rom is INITIAL.
      lva_frete_sem_inf_rom = abap_true.
      CONTINUE.
    ENDIF.

    IF lwa_zlest0223_env_sigam-cancelado IS INITIAL.

      CLEAR: lwa_dados.

      lwa_dados-idtransporte             =  lwa_zlest0223_env_sigam-id_transporte.
      lwa_dados-trecho                   =  lwa_zlest0223_env_sigam-trecho.
      lwa_dados-fornecimento             =  lwa_zlest0223_env_sigam-fornecimento.
      lwa_dados-fornecimentoitem         =  lwa_zlest0223_env_sigam-fornecimento_item.
      lwa_dados-empresa                  =  lwa_zlest0223_env_sigam-empresa.
      lwa_dados-filial                   =  lwa_zlest0223_env_sigam-filial.
      lwa_dados-produto                  =  lwa_zlest0223_env_sigam-produto.
      lwa_dados-safra                    =  lwa_zlest0223_env_sigam-safra.
      lwa_dados-quantidadevinc           =  lwa_zlest0223_env_sigam-qtde_utilizada.
      lwa_dados-valorfretereal           =  lwa_zlest0223_env_sigam-vl_brl.
      lwa_dados-valorfretedolar          =  lwa_zlest0223_env_sigam-vl_usd.

      lwa_dados-ValorPedagioReal         =  lwa_zlest0223_env_sigam-vl_ped_brl.
      lwa_dados-valorPedagioDolar        =  lwa_zlest0223_env_sigam-vl_ped_usd.
      lwa_dados-ValorEstadiaReal         =  lwa_zlest0223_env_sigam-vl_estadia_brl.
      lwa_dados-ValorEstadiaDolar        =  lwa_zlest0223_env_sigam-vl_estadia_usd.
      lwa_dados-ValorDiferencialReal     =  lwa_zlest0223_env_sigam-vl_dif_fre_brl.
      lwa_dados-ValorDiferencialDolar    =  lwa_zlest0223_env_sigam-vl_dif_fre_usd.

      CONDENSE: lwa_dados-valorfretereal,
                lwa_dados-valorfretedolar,
                lwa_dados-ValorPedagioReal,
                lwa_dados-valorPedagioDolar,
                lwa_dados-ValorEstadiaReal,
                lwa_dados-ValorEstadiaDolar,
                lwa_dados-ValorDiferencialReal,
                lwa_dados-ValorDiferencialDolar NO-GAPS.

      READ TABLE lit_DD07T_TP_TRECHO INTO DATA(lwa_tp_trecho) WITH KEY DOMVALUE_L = lwa_zlest0223_env_sigam-tp_trecho_v BINARY SEARCH.
      IF sy-subrc eq 0.
        lwa_dados-descricaotrecho = lwa_tp_trecho-DDTEXT.
        lwa_dados-descricaotrecho = zcl_string=>tira_acentos( i_texto = CONV #( lwa_dados-descricaotrecho ) ).
      ENDIF.

      READ TABLE lit_DD07T_TP_FORN INTO DATA(lwa_tp_forn) WITH KEY DOMVALUE_L = lwa_zlest0223_env_sigam-tp_forn_v BINARY SEARCH.
      IF sy-subrc eq 0.
        lwa_dados-descricaotipofornecimento = lwa_tp_forn-DDTEXT.
        lwa_dados-descricaotipofornecimento = zcl_string=>tira_acentos( i_texto = CONV #( lwa_dados-descricaotipofornecimento ) ).
      ENDIF.

      READ TABLE lit_T173T INTO DATA(lwa_t173t) WITH KEY VSART = lwa_zlest0223_env_sigam-vsart BINARY SEARCH.
      IF sy-subrc eq 0.
        lwa_dados-descricaomodal = lwa_t173t-bezei.
        lwa_dados-descricaomodal = zcl_string=>tira_acentos( i_texto = CONV #( lwa_dados-descricaomodal ) ).
      ENDIF.

      READ TABLE lit_TVTKT INTO DATA(lwa_tvtkt) WITH KEY shtyp = lwa_zlest0223_env_sigam-SHTYP BINARY SEARCH.
      IF sy-subrc eq 0.
        lwa_dados-descricaotipotransporte = lwa_tvtkt-bezei.
        lwa_dados-descricaotipotransporte = zcl_string=>tira_acentos( i_texto = CONV #( lwa_dados-descricaotipotransporte ) ).
      ENDIF.

      lwa_dados-tipofornecimento         =  lwa_zlest0223_env_sigam-vbtyp_v.
      lwa_dados-tipotransporte           =  lwa_zlest0223_env_sigam-shtyp.
      lwa_dados-portodestino             =  lwa_zlest0223_env_sigam-porto_destino.
      lwa_dados-localembarque            =  lwa_zlest0223_env_sigam-local_embarque.
      lwa_dados-localentrega             =  lwa_zlest0223_env_sigam-local_entrega.
      lwa_dados-tipotrecho               =  lwa_zlest0223_env_sigam-tp_trecho.
      lwa_dados-transbordo               =  lwa_zlest0223_env_sigam-transbordo.
      lwa_dados-realizado                =  lwa_zlest0223_env_sigam-realizado.
      lwa_dados-chavereferencia          =  lwa_zlest0223_env_sigam-ch_referencia_rom.
      lwa_dados-nrov                     =  lwa_zlest0223_env_sigam-nr_ov.

      IF lwa_zlest0223_env_sigam-nr_ov IS NOT INITIAL.
        READ TABLE lit_vbak INTO DATA(lwa_vbak) WITH KEY vbeln = lwa_zlest0223_env_sigam-nr_ov BINARY SEARCH.
        IF ( sy-subrc EQ 0 ) AND ( lwa_vbak-auart = 'ZIND' ).
          lwa_dados-industrializacao = abap_true.
        ENDIF.
      ENDIF.

      IF lwa_zlest0223_env_sigam-nr_pedido IS NOT INITIAL.
        READ TABLE lit_ekko INTO DATA(lwa_ekko) WITH KEY ebeln = lwa_zlest0223_env_sigam-nr_pedido BINARY SEARCH.
        IF ( sy-subrc EQ 0 ) AND ( lwa_ekko-bsart = 'ZUB' ).
          lwa_dados-transferencia = abap_true.
        ENDIF.
      ENDIF.

      IF lwa_zlest0223_env_sigam-fornecimento IS NOT INITIAL.
        READ TABLE lit_zlest0108 INTO DATA(lwa_zlest0108) WITH KEY vbeln = lwa_zlest0223_env_sigam-fornecimento BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          lwa_dados-intermunicipal = abap_true.
        ELSE.
          IF lwa_zlest0223_env_sigam-shtyp     = 'Z021' AND "Frete Recebimento
             lwa_zlest0223_env_sigam-tp_trecho = '01'.      "Fora da Filial x Filial
            lwa_dados-intermunicipal = abap_true.
          ENDIF.
        ENDIF.
      ENDIF.

      IF lwa_zlest0223_env_sigam-ch_referencia_rom IS NOT INITIAL.
        READ TABLE lit_zsdt0001 INTO DATA(lwa_zsdt0001) WITH KEY ch_referencia = lwa_zlest0223_env_sigam-ch_referencia_rom BINARY SEARCH.
        IF ( sy-subrc EQ 0 ).
          lwa_dados-tiporomaneio = lwa_zsdt0001-tp_movimento.
        ENDIF.
      ENDIF.

      lwa_dados-nrpedido                 =  lwa_zlest0223_env_sigam-nr_pedido.
      lwa_dados-modal                    =  lwa_zlest0223_env_sigam-vsart.
      CONCATENATE lwa_zlest0223_env_sigam-data_transporte+6(2) '/' lwa_zlest0223_env_sigam-data_transporte+4(2) '/' lwa_zlest0223_env_sigam-data_transporte+0(4) INTO lwa_dados-datadoctransporte.
      CONCATENATE lwa_zlest0223_env_sigam-data_custo+6(2)      '/' lwa_zlest0223_env_sigam-data_custo+4(2)      '/' lwa_zlest0223_env_sigam-data_custo+0(4)      INTO lwa_dados-datadoccusto.
      lwa_dados-doccusto                 =  lwa_zlest0223_env_sigam-fknum.
      lwa_dados-doctransporte            =  lwa_zlest0223_env_sigam-tknum.
      lwa_dados-agentefrete              =  lwa_zlest0223_env_sigam-tdlnr.
      lwa_dados-proprietarioveiculo      =  lwa_zlest0223_env_sigam-propr_veiculo.
      lwa_dados-cancel                   =  lwa_zlest0223_env_sigam-cancelado.

*      PERFORM f_int_sigam_fre_realizado USING 'POST'
*                                               lwa_dados
*                                      CHANGING lva_return.


    ELSEIF lwa_zlest0223_env_sigam-cancelado = 'X'.

      CLEAR: lwa_dados_canc.

      lwa_dados_canc-idtransporte  = lwa_zlest0223_env_sigam-id_transporte.
      lwa_dados_canc-trecho        = lwa_zlest0223_env_sigam-trecho.
      lwa_dados_canc-fornecimento  = lwa_zlest0223_env_sigam-fornecimento.
      lwa_dados_canc-cancel        = lwa_zlest0223_env_sigam-cancelado.

      "PERFORM f_int_sigam_fre_realizado USING 'DELETE'
      "                                        lwa_dados_canc
      "                               CHANGING lva_return.

    ENDIF.

    "Tratamento por grupo
    IF lwa_zlest0223_env_sigam-cancelado IS NOT INITIAL.
      lva_registro_metodo = C_DELETE.
    ELSE.
      lva_registro_metodo = C_POST.
    ENDIF.

    IF _lva_tabix EQ 1.

      CLEAR: lwa_group_send.

      lva_current_id_group    = 1.
      lva_current_metodo      = lva_registro_metodo.

      lwa_group_send-id_group = lva_current_id_group.
      lwa_group_send-metodo   = lva_current_metodo.

    ENDIF.

    IF ( lva_registro_metodo NE lva_current_metodo                    ) OR "Metodo Diferente
       ( lines( lwa_group_send-registros_send[] ) >= lva_max_reg_send ).   "Limite Maximo de Registros por Bloco

      "Gerar Grupo novo para envio
      APPEND lwa_group_send to lit_group_send.

      CLEAR: lwa_group_send.

      lva_current_id_group    = lva_current_id_group + 1.
      lva_current_metodo      = lva_registro_metodo.

      lwa_group_send-id_group = lva_current_id_group.
      lwa_group_send-metodo   = lva_current_metodo.

    ENDIF.

    CASE lwa_group_send-metodo.
      WHEN C_POST.
        APPEND lwa_dados               to lwa_group_send-registros_post.
        APPEND lwa_zlest0223_env_sigam to lwa_group_send-registros_send.
      WHEN C_DELETE.
        APPEND lwa_dados_canc          to lwa_group_send-registros_canc.
        APPEND lwa_zlest0223_env_sigam to lwa_group_send-registros_send.
    ENDCASE.

    IF ( lines( lit_zlest0223_env_sigam[] ) = _lva_tabix ).  "Ultimo registro do Looping
      APPEND lwa_group_send to lit_group_send.
    ENDIF.


  ENDLOOP.

  SORT lit_group_send BY id_group.
  LOOP AT lit_group_send INTO lwa_group_send.

    CASE lwa_group_send-metodo.
      WHEN C_POST.
        PERFORM f_int_sigam_fre_realizado TABLES lwa_group_send-registros_post
                                                 lwa_group_send-registros_send
                                           USING lwa_group_send-metodo.
      WHEN C_DELETE.
        PERFORM f_int_sigam_fre_realizado TABLES lwa_group_send-registros_canc
                                                 lwa_group_send-registros_send
                                           USING lwa_group_send-metodo.
    ENDCASE.
  ENDLOOP.



  IF lva_frete_sem_inf_rom eq abap_true.
    PERFORM f_notifica_suporte_sap.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  F_INT_SIGAM_FRE_REALIZADO
*&---------------------------------------------------------------------*
FORM f_int_sigam_fre_realizado TABLES s_table TYPE STANDARD TABLE
                                      t_zlest0223_env LIKE gt_zlest0223_env
                                USING s_metodo.

  DATA: lit_dados_env_post TYPE TABLE OF ty_dados,
        lit_dados_env_canc TYPE TABLE OF ty_dados_canc.

  DATA: e_json            TYPE string,
        lva_id_referencia TYPE string,
        lwa_referencia    TYPE ty_dados.

  DATA: lva_return TYPE i.

  CLEAR: lit_dados_env_post[], lit_dados_env_post.

  CASE s_metodo.
    WHEN c_post.

      MOVE-CORRESPONDING s_table[] to lit_dados_env_post[].

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = lit_dados_env_post
        RECEIVING
          r_json = e_json.

    WHEN c_delete.

      MOVE-CORRESPONDING s_table[] to lit_dados_env_canc[].

      CALL METHOD /ui2/cl_json=>serialize
        EXPORTING
          data   = lit_dados_env_canc
        RECEIVING
          r_json = e_json.
  ENDCASE.

  IF LINES( t_zlest0223_env ) EQ 1.
    READ TABLE t_zlest0223_env INTO DATA(lwa_zlest0223_env_sigam) INDEX 1.
    IF SY-SUBRC EQ 0.
      lva_id_referencia  = lwa_zlest0223_env_sigam-fornecimento.
    ENDIF.
  ELSE.
    CLEAR: lva_id_referencia.
  ENDIF.

  DATA(_sucess) = abap_false.

  TRY .
      zcl_int_realizado_frete_sigam=>zif_integracao_real_fre_sigam~get_instance(
        )->set_int_real_frete_sigam( i_json = e_json i_metodo = conv #( s_metodo ) i_id_referencia = conv #( lva_id_referencia )  ).
      _sucess = abap_true.
    CATCH zcx_integracao INTO DATA(ex_integra).
      ex_integra->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'S' ).
    CATCH zcx_error INTO DATA(ex_error).    "  "
      ex_error->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'S' ).
  ENDTRY.

  CHECK _sucess = abap_true.

  LOOP AT t_zlest0223_env INTO lwa_zlest0223_env_sigam.

    SELECT SINGLE *
      FROM zlest0223 INTO @DATA(lwa_zlest0223)
     WHERE id_transporte       EQ @lwa_zlest0223_env_sigam-id_transporte
       AND trecho              EQ @lwa_zlest0223_env_sigam-trecho
       AND fornecimento        EQ @lwa_zlest0223_env_sigam-fornecimento
       AND fornecimento_item   EQ @lwa_zlest0223_env_sigam-fornecimento_item
       AND fknum               EQ @lwa_zlest0223_env_sigam-fknum .

    CHECK lwa_zlest0223 IS NOT INITIAL.

    lwa_zlest0223-sincronizado_sigam = 'X'.
    lwa_zlest0223-dt_sincronia_sigam   = sy-datum.
    lwa_zlest0223-hr_sincronia_sigam   = sy-uzeit.

    MODIFY zlest0223 FROM lwa_zlest0223.
    COMMIT WORK.

  ENDLOOP.

ENDFORM.

FORM f_sort_tabelas .

  SORT: gt_likp     BY vbeln,
        "gt_vbfa_vt  by vbelv,
        gt_vttk     BY tknum,
        gt_tvtk     BY shtyp,
        gt_tvtkt    BY shtyp,
        gt_vfkp     BY rebel,
        gt_tvrot    BY route,
        gt_vfsi     BY knumv vbeln posnr,
        gt_curr_brl BY gdatu,
        gt_curr_usd BY gdatu,
        gt_vbfa_nf  BY vbelv,
        gt_lin      BY refkey,
        gt_doc      BY docnum,
        gt_vbak     BY vbeln,
        gt_ekko     BY ebeln,
        gt_lfa1     BY lifnr,
        gt_zgr_docs BY av_vbeln,
        gt_vbpa     BY vbeln parvw,
        gt_vbpa_lf  BY vbeln,
        gt_carta    BY docnum,
        gt_branch   BY branch,
        gt_zcentro  BY centrov_1.

ENDFORM.

FORM f_monta_chave_trecho_ref  USING p_zlest0223         TYPE zlest0223
                            CHANGING c_chave_trecho_ref  TYPE zlest0223-chave_trecho_ref.

  CLEAR: c_chave_trecho_ref.

  CONCATENATE p_zlest0223-id_transporte
              p_zlest0223-trecho
              p_zlest0223-fornecimento
              p_zlest0223-fornecimento_item
              p_zlest0223-fknum
         INTO c_chave_trecho_ref SEPARATED BY '-'.

ENDFORM.

FORM f_get_id_transporte CHANGING p_id_transporte TYPE zlest0223-id_transporte.

  CLEAR: p_id_transporte.

  CALL FUNCTION 'NUMBER_GET_NEXT'
    EXPORTING
      nr_range_nr             = '01'
      object                  = 'Z_ID_TRANS'
    IMPORTING
      number                  = p_id_transporte
    EXCEPTIONS
      interval_not_found      = 1
      number_range_not_intern = 2
      object_not_found        = 3
      quantity_is_0           = 4
      quantity_is_not_1       = 5
      interval_overflow       = 6
      buffer_overflow         = 7
      OTHERS                  = 8.

  IF sy-subrc <> 0.
    MESSAGE 'Objecto Z_ID_TRANS não existe na SNRO' TYPE 'E'.
  ENDIF.

ENDFORM.

FORM f_check_hora_sinc_rom  USING p_0001 TYPE zsdt0001
                         CHANGING p_ok.

  DATA: vr_qtd_hora     TYPE p DECIMALS 2,
        vr_qtd_hora_ref TYPE p DECIMALS 2.

  p_ok = abap_true.

  CHECK p_0001-dt_atualizacao IS NOT INITIAL.

  vr_qtd_hora_ref = 2.

  "Verificar se a mais de duas horas de diferença.
  CLEAR: vr_qtd_hora.
  IF sy-datum EQ p_0001-dt_atualizacao.

    vr_qtd_hora = ( sy-uzeit - p_0001-hr_atualizacao ) / 60.
    vr_qtd_hora = vr_qtd_hora / 60.
    IF vr_qtd_hora <= vr_qtd_hora_ref.
      CLEAR p_ok.
    ENDIF.
  ENDIF.


ENDFORM.

FORM f_consome_saldo_transbordo  USING  p_zlest0223_transb TYPE zlest0223
                                        p_quantidade       TYPE zlest0223-saldo_transb.

  SELECT SINGLE *
    FROM zlest0223 INTO @DATA(lw_zlest0223_update)
   WHERE id_transporte      EQ @p_zlest0223_transb-id_transporte
     AND trecho             EQ @p_zlest0223_transb-trecho
     AND fornecimento       EQ @p_zlest0223_transb-fornecimento
     AND fornecimento_item  EQ @p_zlest0223_transb-fornecimento_item
     AND fknum              EQ @p_zlest0223_transb-fknum.

   CHECK sy-subrc EQ 0.

   lw_zlest0223_update-saldo_transb = lw_zlest0223_update-saldo_transb - p_quantidade.
   MODIFY zlest0223 FROM lw_zlest0223_update.

ENDFORM.

FORM f_notifica_suporte_sap .

  DATA: VL_TITULO    TYPE STRING.
  DATA: LIT_ZSDT0105 TYPE TABLE OF ZSDT0105.
  DATA: LIT_ZSDT0296 TYPE TABLE OF ZSDT0296.
  DATA: LIT_ZMAIL    TYPE TABLE OF ZMAIL.
  DATA: IT_HTML      TYPE TABLE OF W3HTML INITIAL SIZE 0.


  DATA: OBJPACK     TYPE TABLE OF SOPCKLSTI1,
        LWA_OBJPACK TYPE SOPCKLSTI1.

  DATA: OBJHEAD     TYPE TABLE OF SOLISTI1.
  DATA: OBJBIN_ORD  TYPE TABLE OF SOLISTI1.
  DATA: OBJBIN_LOG  TYPE TABLE OF SOLISTI1.
  DATA: OBJBIN_ANN  TYPE SOLISTI1.
  DATA: OBJBIN      TYPE TABLE OF SOLISTI1.
  DATA: WA_OBJBIN   TYPE SOLISTI1.
  DATA: CONTENT_HEX TYPE STANDARD TABLE OF SOLIX.
  DATA: OBJTXT      TYPE TABLE OF SOLISTI1.
  DATA: RECLIST     TYPE TABLE OF SOMLRECI1.
  DATA: LWA_RECLIST TYPE SOMLRECI1.
  DATA: DOC_CHNG    TYPE SODOCCHGI1.
  DATA: TAB_LINES   TYPE SY-TABIX.

  DATA: LV_VALOR TYPE STRING.

  DATA: LV_TITLE_EMAIL TYPE STRING.

  DATA: VL_NMDFE    TYPE STRING,
        VL_DOCNUM   TYPE STRING,
        VL_FILIAL   TYPE STRING,
        VL_DATA_AUT TYPE STRING,
        VL_MSG_RET  TYPE STRING.

  DEFINE CONC_HTML.
    LV_VALOR = &1.

    CALL FUNCTION 'ZHTML_ADD'
      EXPORTING
        I_TEXTO = LV_VALOR
      TABLES
        IT_HTML = IT_HTML.
  END-OF-DEFINITION.


  CLEAR: RECLIST[].

  "Determinação Destinatarios
  LWA_RECLIST-RECEIVER = 'wellington.pereira@amaggi.com.br'.
  LWA_RECLIST-REC_TYPE = 'U'.
  APPEND LWA_RECLIST TO RECLIST.

  LWA_RECLIST-RECEIVER = 'leila.mara@amaggi.com.br'.
  LWA_RECLIST-REC_TYPE = 'U'.
  APPEND LWA_RECLIST TO RECLIST.

  LV_TITLE_EMAIL = 'FRETE REALIZADO SIGAM - SEM INFORMAÇÕES ROMANEIO'.

  VL_TITULO = 'Existem Fretes Realizado para enviar ao SIGAM, sem informação de Romaneio(Chave Referencia) - Programa JOB ZSDR0129 - Tabela ZLEST0223  Campo CH_REFERENCIA_ROM Vazio!'.


  "Monta Corpo Email
  CONC_HTML '<html>'.
  CONC_HTML '<head><title>'.
  CONC_HTML    LV_TITLE_EMAIL.
  CONC_HTML '</title><meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1"></head>'.
  CONC_HTML '<body bgcolor="#f5f1ff" leftmargin="0" topmargin="0" marginwidth="0" marginheight="0">'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ff0000 size=4><STRONG>'.
  CONC_HTML    VL_TITULO.
  CONC_HTML '</STRONG></FONT></DIV><BR>'.
  CONC_HTML '<FONT face=Verdana color=#0000ff size=2>'.
  CONC_HTML '<BR>'.
  CONC_HTML '<BR>'.
  CONC_HTML '<BR>'.
  CONC_HTML '<DIV align=left>'.
  CONC_HTML '<DIV align=center><FONT face=Verdana color=#ffaaaa size=1><STRONG>E-mail gerado automáticamente pelo sistema</STRONG></FONT></DIV>'.

  CONC_HTML '</DIV>'.
  CONC_HTML '<BR>'.
  CONC_HTML '</body>'.
  CONC_HTML '</html>'.

  "Corpo
  DOC_CHNG-OBJ_NAME  = LV_TITLE_EMAIL.
  DOC_CHNG-OBJ_DESCR = LV_TITLE_EMAIL.
  DOC_CHNG-NO_CHANGE = 'X'.

  CLEAR LWA_OBJPACK-TRANSF_BIN.
  LWA_OBJPACK-HEAD_START = 1.
  LWA_OBJPACK-HEAD_NUM = 0.
  LWA_OBJPACK-BODY_START = 1.
  LWA_OBJPACK-BODY_NUM = 99999.
  LWA_OBJPACK-DOC_TYPE = 'HTM'.
  APPEND LWA_OBJPACK TO OBJPACK.

  "Enviar
  CALL FUNCTION 'SO_NEW_DOCUMENT_ATT_SEND_API1'
    EXPORTING
      DOCUMENT_DATA              = DOC_CHNG
      PUT_IN_OUTBOX              = 'X'
      COMMIT_WORK                = 'X'
    TABLES
      PACKING_LIST               = OBJPACK
      CONTENTS_TXT               = IT_HTML
      RECEIVERS                  = RECLIST
    EXCEPTIONS
      TOO_MANY_RECEIVERS         = 1
      DOCUMENT_NOT_SENT          = 2
      OPERATION_NO_AUTHORIZATION = 4
      OTHERS                     = 99.

ENDFORM.

FORM f_set_parceiros_from_vtts  USING p_vtts TYPE vtts
                             CHANGING p_zlest0223 TYPE zlest0223.

  DATA(lob_zcl_fornecedores) = NEW zcl_fornecedores( ).
  DATA(lob_zcl_clientes)     = NEW zcl_clientes( ).

*---------------------------------------------------------------------------------*
*   Local de chegada / Entrega
*---------------------------------------------------------------------------------*

  if p_vtts-lifnz is NOT INITIAL.

    p_zlest0223-porto_destino  = p_vtts-lifnz.

    SELECT SINGLE *
      FROM lfa1 INTO @DATA(lwa_lfa1_lr)
     WHERE lifnr EQ @p_vtts-lifnz.

    IF SY-SUBRC EQ 0.
      TRY .
        lob_zcl_clientes->zif_parceiros~set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj             = CONV #( lwa_lfa1_lr-stcd1 )
                                                                            i_insc_estatual    = CONV #( lwa_lfa1_lr-stcd3 )
                                                                            i_agnorar_bloqueio = abap_true
                                                                   )->get_id_parceiro( IMPORTING e_parceiro = p_zlest0223-local_entrega ).

      CATCH zcx_parceiros INTO DATA(zcx_parc).
      ENDTRY.
    ENDIF.

  elseif p_vtts-kunnz is NOT INITIAL.

    p_zlest0223-local_entrega = p_vtts-kunnz.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(lwa_kna1_z1)
     WHERE kunnr EQ @p_vtts-kunnz.

    IF SY-SUBRC EQ 0.
      TRY .
        lob_zcl_fornecedores->zif_parceiros~set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj             = CONV #( lwa_kna1_z1-stcd1 )
                                                                                i_insc_estatual    = CONV #( lwa_kna1_z1-stcd3 )
                                                                                i_agnorar_bloqueio = abap_true
                                                                   )->get_id_parceiro( IMPORTING e_parceiro = p_zlest0223-porto_destino ).

      CATCH zcx_parceiros INTO zcx_parc.
      ENDTRY.
    ENDIF.
  endif.

*---------------------------------------------------------------------------------*
*   Local de Partida
*---------------------------------------------------------------------------------*
  if p_vtts-lifna is NOT INITIAL.
    p_zlest0223-local_embarque     = p_vtts-lifna.
  elseif p_vtts-kunna is NOT INITIAL.

    SELECT SINGLE *
      FROM kna1 INTO @DATA(lwa_kna1_pc)
     WHERE kunnr EQ @p_vtts-kunna.

    IF SY-SUBRC EQ 0.
      TRY .
        lob_zcl_fornecedores->zif_parceiros~set_parceiro_cnpj_cpf_ie( EXPORTING i_cnpj             = CONV #( lwa_kna1_pc-stcd1 )
                                                                                i_insc_estatual    = CONV #( lwa_kna1_pc-stcd3 )
                                                                                i_agnorar_bloqueio = abap_true
                                                                   )->get_id_parceiro( IMPORTING e_parceiro = p_zlest0223-local_embarque ).
      CATCH zcx_parceiros INTO zcx_parc.
      ENDTRY.
    ENDIF.

  endif.

ENDFORM.

FORM f_selec_outros_val_transp .

  DATA lv_data_tmp TYPE char08.

  DATA lv_data TYPE sy-datum.

  " caso nao esteja preenchido....
  IF gv_dias_busca IS INITIAL.

    gv_dias_busca = 5.

  ENDIF.

  lv_data = sy-datum.

  SUBTRACT gv_dias_busca FROM lv_data.

  SELECT *
     FROM ZPFE_LOTE_ITEM INTO CORRESPONDING FIELDS OF TABLE gt_zpfe_lote_item
    WHERE DT_TRANSACAO >= lv_data
      AND chvid in ('6','24'). "6 - Estadia / 24 - Diferencial de Frete

  DELETE gt_zpfe_lote_item WHERE tknum IS INITIAL.

  LOOP AT gt_zpfe_lote_item ASSIGNING FIELD-SYMBOL(<fs_pfe_lote_item>).

    lv_data_tmp = <fs_pfe_lote_item>-dt_transacao+6(2) && <fs_pfe_lote_item>-dt_transacao+4(2) && <fs_pfe_lote_item>-dt_transacao(4).

    CALL FUNCTION 'CONVERSION_EXIT_INVDT_INPUT'
      EXPORTING
        input  = lv_data_tmp
      IMPORTING
        output = <fs_pfe_lote_item>-gdatu.

  ENDLOOP.

  CHECK gt_zpfe_lote_item[] is NOT INITIAL.

  SELECT * FROM tcurr
    INTO TABLE gt_curr_other_brl
    FOR ALL ENTRIES IN gt_zpfe_lote_item
      WHERE kurst = 'B'
        AND fcurr = 'BRL'
        AND tcurr = 'USD'
        AND gdatu = gt_zpfe_lote_item-gdatu.

  SELECT *
    FROM zlest0223 INTO TABLE gt_0223_outros_val
     FOR ALL ENTRIES IN gt_zpfe_lote_item
    WHERE TKNUM = gt_zpfe_lote_item-tknum.

ENDFORM.

FORM f_proce_outros_val_transp.

  DATA: lva_estadia_brl     TYPE zlest0223-vl_estadia_brl,
        lva_estadia_usd     TYPE zlest0223-vl_estadia_usd,
        lva_diferencial_brl TYPE zlest0223-vl_dif_fre_brl,
        lva_diferencial_usd TYPE zlest0223-vl_dif_fre_usd.

   SORT: gt_curr_other_brl BY gdatu.

  LOOP AT gt_0223_outros_val ASSIGNING FIELD-SYMBOL(<fs_outros_val>).

    DATA(lva_alterou) = abap_false.

    CLEAR: lva_estadia_brl, lva_estadia_usd, lva_diferencial_brl, lva_diferencial_usd.

    "Só devemos gravar os valores de estadia e diferencial, na primeira remessa vinculada a TK
    DATA(_ja_gravou_dif_estadia) = abap_false.
    LOOP AT gt_0223_outros_val INTO DATA(LWA_0223_TMP) WHERE tknum = <fs_outros_val>-tknum
                                                         and ( vl_estadia_brl NE 0 or vl_dif_fre_brl NE 0 ).

      IF NOT ( ( LWA_0223_TMP-ID_TRANSPORTE       EQ <fs_outros_val>-ID_TRANSPORTE     ) AND
               ( LWA_0223_TMP-TRECHO              EQ <fs_outros_val>-TRECHO            ) AND
               ( LWA_0223_TMP-FORNECIMENTO        EQ <fs_outros_val>-FORNECIMENTO      ) AND
               ( LWA_0223_TMP-FORNECIMENTO_ITEM   EQ <fs_outros_val>-FORNECIMENTO_ITEM ) AND
               ( LWA_0223_TMP-FKNUM               EQ <fs_outros_val>-FKNUM ) ).
        _ja_gravou_dif_estadia = abap_true.
        EXIT.
      ENDIF.
    ENDLOOP.

    IF _ja_gravou_dif_estadia EQ abap_true.
      CONTINUE.
    ENDIF.

    LOOP AT gt_zpfe_lote_item INTO DATA(lwa_pfe_lote_item) WHERE tknum eq <fs_outros_val>-tknum.
      CASE lwa_pfe_lote_item-chvid.
        WHEN '6'. "Estadia
          add lwa_pfe_lote_item-vl_transacao to lva_estadia_brl.
        WHEN '24'. "Diferencial de Frete
          add lwa_pfe_lote_item-vl_transacao to lva_diferencial_brl.
      ENDCASE.
    ENDLOOP.



    READ TABLE gt_curr_other_brl ASSIGNING FIELD-SYMBOL(<fs_curr_brl>)
       WITH KEY gdatu = lwa_pfe_lote_item-gdatu BINARY SEARCH.

    IF sy-subrc EQ 0.
      IF <fs_curr_brl>-ukurs <> 0.
        lva_diferencial_usd = ABS( lva_diferencial_brl / <fs_curr_brl>-ukurs ).
        lva_estadia_usd     = ABS( lva_estadia_brl     / <fs_curr_brl>-ukurs ).
      ENDIF.
    ENDIF.

    IF ( <fs_outros_val>-vl_dif_fre_brl NE lva_diferencial_brl ) or
       ( <fs_outros_val>-vl_dif_fre_usd NE lva_diferencial_usd ) or
       ( <fs_outros_val>-vl_estadia_brl NE lva_estadia_brl ) or
       ( <fs_outros_val>-vl_estadia_usd NE lva_estadia_usd ).

      <fs_outros_val>-vl_dif_fre_brl = lva_diferencial_brl.
      <fs_outros_val>-vl_dif_fre_usd = lva_diferencial_usd.
      <fs_outros_val>-vl_estadia_brl = lva_estadia_brl.
      <fs_outros_val>-vl_estadia_usd = lva_estadia_usd.

      lva_alterou = abap_true.
    ENDIF.

    IF lva_alterou = abap_true.

      IF NOT ( ( <fs_outros_val>-cancelado EQ abap_true ) AND ( <fs_outros_val>-sincronizado_sigam = abap_true ) ).
        <fs_outros_val>-sincronizado_sigam  = space.
      ENDIF.

      <fs_outros_val>-dt_atualizacao      = sy-datum.
      <fs_outros_val>-hr_atualizacao      = sy-uzeit.
      <fs_outros_val>-us_atualizacao      = sy-uname.

      MODIFY zlest0223 FROM <fs_outros_val>.

    ENDIF.

  ENDLOOP.

ENDFORM.

FORM f_delete_base .

  CHECK 1 = 2.

  delete FROM zlest0223.

ENDFORM.
