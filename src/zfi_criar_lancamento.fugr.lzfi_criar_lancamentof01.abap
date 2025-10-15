*----------------------------------------------------------------------*
***INCLUDE LZFI_CRIAR_LANCAMENTOF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form f_grava_lancamento
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> I_EDIT
*&      <-- E_NUMER_LANCAMENTO
*&---------------------------------------------------------------------*
FORM f_grava_lancamento  USING    p_edit TYPE zfi_edit
                                  p_026  TYPE tb_026
                         CHANGING p_numer_lancamento.
  TYPES:
    BEGIN OF ty_lanc_ver,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      mont_mi         TYPE zfit0026-mont_mi,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      uname           TYPE zfit0026-uname,
      data_registro   TYPE zfit0026-data_registro,
      bukrs           TYPE zfit0026-bukrs,
      obj_key         TYPE zfit0026-obj_key,
      docnum          TYPE zfit0026-docnum,
      zterm           TYPE zfit0026-zterm,
      doc_fatura      TYPE zfit0026-doc_fatura,
      data_pgto       TYPE zfit0026-data_pgto,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
      vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
      razao_especial  TYPE zfit0026-razao_especial,
      observacao(255) TYPE c,
      ajuste          TYPE zfit0026-ajuste,
      mont_moeda_fix  TYPE zfit0026-mont_moeda,
      nfenum          TYPE j_1bnfdoc-nfenum,
      edit(4)         TYPE c,
      gera(4)         TYPE c,
      status_doc(4)   TYPE c,
      excluir(4)      TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      num_comp_adiant TYPE zfit0026-num_comp_adiant,
    END OF ty_lanc_ver,

    BEGIN OF ty_lanc,
      vbeln           TYPE zfit0026-vbeln,
      seq             TYPE zfit0026-seq,
      data_venc       TYPE zfit0026-data_venc,
      moeda           TYPE zfit0026-moeda,
      mont_moeda      TYPE zfit0026-mont_moeda,
      taxa            TYPE zfit0026-taxa,
      forma_pag       TYPE zfit0026-forma_pag,
      status          TYPE zfit0026-status,
      uname           TYPE zfit0026-uname,
      data_registro   TYPE zfit0026-data_registro,
      obj_key         TYPE zfit0026-obj_key,
      docnum          TYPE zfit0026-docnum,
      razao_especial  TYPE zfit0026-razao_especial,
      mont_moeda_fix  TYPE zfit0026-mont_moeda,
      mont_moeda_parc TYPE zfit0026-mont_moeda,
      zterm           TYPE zfit0026-zterm,
      razao           TYPE c LENGTH 8,
      nfenum          TYPE j_1bnfdoc-nfenum,
      data_pgto       TYPE zfit0026-data_pgto,
      vlr_multa_rbdo  TYPE zfit0026-vlr_multa_rbdo,
      vlr_juros_rbdo  TYPE zfit0026-vlr_juros_rbdo,
      mont_mi         TYPE zfit0026-mont_mi,
      mont_rbdo       TYPE zfit0026-mont_rbdo,
      vlr_multa_calc  TYPE zfit0026-vlr_multa_calc,
      vlr_juros_calc  TYPE zfit0026-vlr_juros_calc,
      doc_fatura      TYPE zfit0026-doc_fatura,
      bukrs_vf        TYPE vbak-bukrs_vf,
      edit(4)         TYPE c,
      gera(4)         TYPE c,
      estor(4)        TYPE c,
      status_doc(4)   TYPE c,
      excluir(4)      TYPE c,
      observacao(255) TYPE c,
      ajuste(1)       TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      vlr_desc_mult   TYPE zfit0026-vlr_desc_mult,
      vlr_desc_jros   TYPE zfit0026-vlr_desc_jros,
      pgto_ant(15)    TYPE c,
      num_comp_adiant TYPE zfit0026-num_comp_adiant, "ZSD - API lanc.Aut. Acerto Insumos SIGAM pt2- BG #115337
    END OF ty_lanc,

    BEGIN OF ty_saida,
      bukrs_vf        TYPE  vbak-bukrs_vf,
      auart           TYPE  vbak-auart,
      kunnr           TYPE  vbak-kunnr,
      vkbur           TYPE  vbak-vkbur,
      vbeln           TYPE  vbak-vbeln,
      erdat           TYPE  vbak-erdat,
      waerk           TYPE  vbak-waerk,
      knumv           TYPE  vbak-knumv,
      netwr           TYPE  vbak-netwr,
      netwr_l         TYPE  vbap-netwr,
      mwsbp           TYPE  vbap-mwsbp,
      zlsch           TYPE  vbkd-zlsch,
      kurrf           TYPE  vbkd-kurrf,
      valdt           TYPE  vbkd-valdt,
      name1           TYPE  kna1-name1,
      werks           TYPE  vbap-werks,
      zterm           TYPE  vbkd-zterm,
      text1           TYPE t052u-text1,
      meio_pgmto      TYPE dd07v-ddtext,
      edit(4)         TYPE  c,
      visual(4)       TYPE  c,
      observacao(255) TYPE c,
      rec_vlr_total   TYPE zfit0026-rec_vlr_total,
      pgto_ant(15)    TYPE c,
    END OF ty_saida,

    BEGIN OF ty_cont_seq,
      vbeln TYPE zfit0026-vbeln,
      seq   TYPE zfit0026-seq,
    END OF ty_cont_seq.


  DATA: wa_vbak_valor TYPE vbak.
  DATA: p_erro(1)        TYPE c,
        edit             TYPE c,
        rbutton_acerto   TYPE c,
        rbutton_deposito TYPE c.

  DATA: p_zid       TYPE numc10,
        cont        TYPE numc5,
        zfit0026_wa TYPE zfit0026,
        soma        TYPE zfit0026-mont_moeda,
        obj_key     TYPE zfit0026-obj_key,

        texto_01    TYPE c LENGTH 100 VALUE 'Valor do montante menor do que o valor',
        texto_02    TYPE c LENGTH 100 VALUE 'do lançamento!',

        texto_03    TYPE c LENGTH 100 VALUE 'Não existe montante suficiente',
        texto_04    TYPE c LENGTH 100 VALUE 'para esse lançamento',
        it_lanc_ver TYPE TABLE OF ty_lanc_ver,
        it_lanc     TYPE TABLE OF ty_lanc,
        wa_lanc_ver TYPE ty_lanc_ver,
        wa_lanc     TYPE ty_lanc,
        p_ins       TYPE c,
        it_zsdt0041 TYPE TABLE OF zsdt0041,
        wa_zsdt0041 TYPE zsdt0041,
        it_zsdt0040 TYPE TABLE OF zsdt0040,
        wa_zsdt0040 TYPE zsdt0040,
        it_saida    TYPE TABLE OF ty_saida,
        wa_saida    TYPE ty_saida,
        it_edit     TYPE TABLE OF zfi_edit,
        it_cont_seq TYPE TABLE OF ty_cont_seq,
        wa_cont_seq TYPE ty_cont_seq.

  DATA: razao_especial TYPE c,
        l_status(1)    TYPE c,
        l_message(64)  TYPE c.

  DATA: it_zib_contabil_chv TYPE STANDARD TABLE OF zib_contabil_chv,
        wa_zib_contabil_chv TYPE zib_contabil_chv,
        it_z0159            TYPE STANDARD TABLE OF zsdt0159,
        wa_z0159            TYPE zsdt0159,
        r_devo_recu         TYPE RANGE OF auart.

  DATA: vl_gdatu     TYPE gdatu_inv.


  DATA: vsoma    TYPE zfit0026-mont_moeda,
        c_vbeln  TYPE zfit0026-vbeln,
        c_bukrs  TYPE zfit0026-bukrs,
        texto_05 TYPE c LENGTH 100 VALUE 'Valor do Montante menor do',
        texto_06 TYPE c LENGTH 100 VALUE 'que a soma total dos lançamentos',
        texto_07 TYPE c LENGTH 100 VALUE 'Ordem de venda não possue saldo para o lançamento',
        texto_08 TYPE c LENGTH 100 VALUE 'Referencia não possue saldo para o lançamento'.
  CLEAR: p_erro.
  c_vbeln = wa_lanc-vbeln.
  c_bukrs = wa_lanc-bukrs_vf.

  FREE: l_status,
        l_message.

  DATA(obj_auart) = NEW zcl_taxa_curva( ).
  r_devo_recu = obj_auart->get_auart( 'ZHEDGEDEVO/RECU' ).

  DATA: obj_zcl_util_sd TYPE REF TO zcl_util_sd.
  CREATE OBJECT obj_zcl_util_sd.
*  IF WA_LANC-DOC_FATURA IS NOT INITIAL.
*    SELECT VBELN SEQ DATA_VENC MOEDA  MONT_MOEDA TAXA MONT_MI FORMA_PAG
*         STATUS UNAME DATA_REGISTRO BUKRS OBJ_KEY DOCNUM ZTERM DOC_FATURA DATA_PGTO
*         VLR_MULTA_RBDO  VLR_JUROS_RBDO MONT_RBDO VLR_MULTA_CALC VLR_JUROS_CALC
*         OBSERVACAO AJUSTE REC_VLR_TOTAL VLR_DESC_JROS VLR_DESC_MULT
*        FROM ZFIT0026
*        INTO CORRESPONDING FIELDS OF TABLE IT_LANC_VER
*      WHERE VBELN EQ  C_VBELN
*        AND DOC_FATURA EQ WA_LANC-DOC_FATURA.
*
*  ELSE.
*    SELECT VBELN SEQ DATA_VENC MOEDA  MONT_MOEDA TAXA MONT_MI FORMA_PAG
*           STATUS UNAME DATA_REGISTRO BUKRS OBJ_KEY DOCNUM ZTERM DOC_FATURA DATA_PGTO
*           VLR_MULTA_RBDO  VLR_JUROS_RBDO MONT_RBDO VLR_MULTA_CALC VLR_JUROS_CALC
*           OBSERVACAO AJUSTE REC_VLR_TOTAL VLR_DESC_JROS VLR_DESC_MULT
*          FROM ZFIT0026
*          INTO CORRESPONDING FIELDS OF TABLE IT_LANC_VER
*        WHERE VBELN EQ  C_VBELN.
*  ENDIF.

  CLEAR: vl_gdatu.
****Selecionar
** Inicio de alteração - CS0982743 - FMartins - 25/04/2022
*  IF wa_lanc-doc_fatura IS NOT INITIAL.
**   IF p_edit-doc_fatura IS NOT INITIAL.
*** Fim de alteração - CS0982743 - FMartins - 25/04/2022
*    SELECT vbeln seq data_venc moeda  mont_moeda taxa mont_mi forma_pag
*         status uname data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
*         vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
*         observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult
*        FROM zfit0026
*        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
*      WHERE vbeln EQ  p_edit-vbeln
*        AND doc_fatura EQ p_edit-doc_fatura.
*
*  ELSE.
  SELECT vbeln seq data_venc moeda  mont_moeda taxa mont_mi forma_pag
         status uname data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
         vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
         observacao ajuste rec_vlr_total vlr_desc_jros vlr_desc_mult
        FROM zfit0026
        INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
      WHERE vbeln EQ  p_edit-vbeln.
*  ENDIF.

****Totalizar montante recebido.
  LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln = c_vbeln.
    vsoma = vsoma + wa_lanc_ver-mont_moeda.
  ENDLOOP.

  IF p_edit-doc_fatura IS NOT INITIAL.
    wa_lanc-mont_moeda_fix = p_edit-total_recebido.
  ELSE.
    wa_lanc-mont_moeda_fix = wa_lanc-mont_moeda_parc.
  ENDIF.

******  Consultar saldo total e parcial da ordem de venda e da referencia.
*  IF WA_LANC_VER-DOC_FATURA IS INITIAL.
*****   Buscar o saldo total e parcial da OV.
*    ZCL_DADOS_OV=>I_VLR_OV(
*                 EXPORTING
*           I_VBELN = WA_LANC_VER-VBELN
*                 IMPORTING
**           E_VLR_TOTAL = TOTAL_OV
*           E_VLR_PARCIAL = WA_LANC-MONT_MOEDA_FIX ).
*  ELSE.
*****     Buscar o saldo total e parcial da referencia.
*    ZCL_DADOS_OV=>I_VLR_REFERENCIA_OV(
*           EXPORTING
*     I_VBELN = WA_LANC_VER-VBELN
*     I_VBELNN = WA_LANC_VER-DOC_FATURA
*           IMPORTING
**     E_VLR_TOTAL = TOTAL_OV
*     E_VLR_PARCIAL = WA_LANC-MONT_MOEDA_FIX ).
*  ENDIF.
******************************************************************

*  IF p_edit-AJUSTE NE 'X' AND WA_LANC-DOCNUM EQ 0.     "Comentado Aoenning
*    IF ( VSOMA > WA_LANC-MONT_MOEDA_FIX ).
*      MESSAGE E888(SABAPDOCU) WITH TEXTO_05 TEXTO_06.
*      EXIT.
*    ENDIF.
*  ENDIF.

  IF p_edit-ajuste NE 'X' AND wa_lanc-docnum EQ 0.     "Comentado Aoenning
    IF ( p_edit-mont_moeda > wa_lanc-mont_moeda_fix ).
      IF p_edit-doc_fatura IS INITIAL.
        MESSAGE e888(sabapdocu) WITH texto_07.
        EXIT.
      ELSE.
        MESSAGE e888(sabapdocu) WITH texto_08.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.


*  "Só pode ser negativo, quando for lançamento de ajuste.
*  IF ( p_edit-mont_moeda < 0 ).
*    IF p_edit-ajuste NE 'X'.
*      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Valor não pode ser Negativo!'.
*      txtopen = abap_true.
*      p_erro = abap_true.
*      PERFORM editor_text.
*      EXIT.
*    ENDIF.
*  ENDIF.
*
*  IF ( p_edit-data_pgto EQ ' ' ).
*    MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Falta preencher o campo data de pagamento!'.
*    txtopen = abap_true.
*    p_erro = abap_true.
*    PERFORM editor_text.
*    EXIT.
*  ENDIF.

  IF p_edit-rec_vlr_total IS NOT INITIAL.
*    IF p_edit-MONT_MOEDA NE WA_SAIDA-NETWR.

    IF p_edit-valor IS NOT INITIAL.
      IF p_edit-total_recebido NE p_edit-mont_moeda.
*        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante da OV deve ser igual ao montante parcial da referencia'.
*        p_erro = abap_true.
*        txtopen = abap_true.
*        PERFORM editor_text.
*        EXIT.
      ENDIF.

      IF ( p_edit-mont_moeda > p_edit-total_recebido ).
*        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante é maior que o valor da referencia'.
*        p_erro = abap_true.
*        txtopen = abap_true.
*        PERFORM editor_text.
*        EXIT.
      ENDIF.

    ELSE.

      IF ( p_edit-mont_moeda > wa_lanc-mont_moeda_parc ).
*        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante é maior que o valor da OV'.
*        p_erro = abap_true.
*        txtopen = abap_true.
*        PERFORM editor_text.
*        EXIT.
      ENDIF.

      IF p_edit-mont_moeda NE wa_lanc-mont_moeda_parc.
*        MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Montante da OV deve ser igual ao montante parcial'.
*        p_erro = abap_true.
*        txtopen = abap_true.
*        PERFORM editor_text.
*        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

*-CS2021000297 - 29.07.2021 - JT - inicio
  IF p_edit-data_pgto IS NOT INITIAL.
    CALL FUNCTION 'Z_CONTROLE_FECHAMES'
      EXPORTING
        i_bukrs  = p_edit-bukrs
        i_data   = p_edit-data_pgto
      IMPORTING
        e_status = l_status
        e_messa  = l_message
      EXCEPTIONS
        error    = 1
        OTHERS   = 2.

    IF l_status = 'E'.
*      DATA(l_mess1) = l_message(30).
*      DATA(l_mess2) = l_message+30(34).
*      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH l_mess1 l_mess2.
*      txtopen = abap_true.
*      p_erro = abap_true.
*      PERFORM editor_text.
*      EXIT.
    ENDIF.
  ENDIF.
*-CS2021000297 - 29.07.2021 - JT - fim

*  ELSE.

*  IF ( ( p_edit IS INITIAL )          OR
*     ( p_edit-data_venc IS INITIAL )  OR
*     ( p_edit-moeda IS INITIAL )      OR
*     ( p_edit-forma_pag IS INITIAL  ) OR
*     ( p_edit-taxa IS INITIAL  )      OR
*     ( p_edit-mont_rbdo IS INITIAL )  OR
*     ( p_edit-zterm IS INITIAL ) )    AND
*     ( p_edit-moeda NE 'BRL').

*    MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Obrigatório preencher todos os campos!'.
*    txtopen = abap_true.
*    p_erro = abap_true.
*    PERFORM editor_text.
*    EXIT.
*      IF p_edit-MONT_MOEDA IS INITIAL AND p_edit-VLR_JUROS_CALC IS INITIAL.

*        MESSAGE S888(SABAPDOCU) DISPLAY LIKE 'E' WITH 'Obrigatório preencher todos os campos!'.

*      ENDIF.

*  ELSE.



*    IF wa_t052-zdart EQ 'B'.
*      IF  ( p_edit-nfenum IS INITIAL ).
**        block = abap_false.
**        MESSAGE 'Favor informar  Referencia(Nfe)!' TYPE 'E'.
**        p_erro = abap_true.
**        PERFORM editor_text.
**        EXIT.
*      ENDIF.
*    ENDIF.

  IF ( p_edit-data_pgto IS INITIAL ) .
*      block = abap_false.
*      MESSAGE 'Favor informar Data Pagamento!' TYPE 'E'.
*      p_erro = abap_true.
*      PERFORM editor_text.
*      EXIT.
  ENDIF.

  IF  ( p_edit-mont_rbdo IS INITIAL ).
*      block = abap_false.
*      MESSAGE 'Favor informar Montante Recebido!' TYPE 'E'.
*      p_erro = abap_true.
*      PERFORM editor_text.
*      EXIT.
  ENDIF.

  IF ( p_edit-vbeln IS INITIAL ).
*      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Clicar em "Novo"'.
  ELSEIF NOT ( p_edit-taxa IS INITIAL ) AND ( wa_lanc-moeda EQ 'BRL').
*      MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH 'Moeda BRL, não é necessário a TAXA'.
*      p_erro = abap_true.
*      CLEAR: p_edit-mont_moeda.
  ELSE.

*        CLEAR p_editOR.
*        CALL METHOD EDITOR->GET_TEXT_AS_STREAM( IMPORTING TEXT = IT_EDITOR ).
*        CLEAR: p_edit-OBSERVACAO.
*        LOOP AT IT_EDITOR INTO p_editOR.
*          p_edit-OBSERVACAO = |{ p_edit-OBSERVACAO } { p_editOR-LINE }|.
*        ENDLOOP.
**        READ TABLE IT_EDITOR INTO p_editOR INDEX 1.

    SELECT COUNT(*)
      INTO cont
      FROM zfit0026
    WHERE vbeln EQ p_edit-vbeln.

    IF ( edit EQ 'X' ).

      IF ( cont EQ 0 ).

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZID_LANC'
          IMPORTING
            number      = p_zid.

        CLEAR: obj_key.

        zfit0026_wa-zid_lanc         = p_zid.
        zfit0026_wa-seq              = cont + 1.
        zfit0026_wa-vbeln            = p_edit-vbeln.
        zfit0026_wa-data_venc        = p_edit-data_venc.
        zfit0026_wa-moeda            = p_edit-moeda.
        zfit0026_wa-bukrs            = p_edit-bukrs.
        zfit0026_wa-zterm            = p_edit-zterm.
        zfit0026_wa-status           = p_edit-status.
        zfit0026_wa-data_pgto        = p_edit-data_pgto.
        zfit0026_wa-mont_rbdo        = p_edit-mont_rbdo.
        zfit0026_wa-rec_vlr_total    = p_edit-rec_vlr_total.
        zfit0026_wa-observacao       = p_edit-observacao.

        IF p_edit-ajuste IS NOT INITIAL AND p_edit-mont_moeda EQ 0.
          zfit0026_wa-vlr_desc_jros   = p_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_desc_mult   = p_edit-vlr_multa_rbdo.
        ELSE.
          zfit0026_wa-vlr_juros_rbdo   = p_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_multa_rbdo   = p_edit-vlr_multa_rbdo.
        ENDIF.


        IF p_edit-ajuste IS INITIAL AND p_edit-mont_moeda NE 0. "
          zfit0026_wa-vlr_juros_calc   = p_edit-vlr_juros_calc.
          zfit0026_wa-vlr_multa_calc   = p_edit-vlr_multa_calc.
        ELSE.
          zfit0026_wa-vlr_juros_calc   = ' '.
          zfit0026_wa-vlr_multa_calc   = ' '.
        ENDIF.

        zfit0026_wa-doc_fatura       = p_edit-doc_fatura.


        zfit0026_wa-mont_moeda    = p_edit-mont_moeda.

        IF ( p_edit-moeda EQ 'BRL' ) AND ( p_edit-taxa IS INITIAL ).
          IF p_edit-ajuste IS INITIAL.
            zfit0026_wa-taxa          = 1.
          ELSE.
            "BUSCA A TAXA
            obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
            obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
            obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

            MOVE  p_edit-data_pgto TO vl_gdatu.
            obj_zcl_util_sd->set_data( vl_gdatu ).
            zfit0026_wa-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

            IF zfit0026_wa-taxa IS INITIAL.
              zfit0026_wa-taxa          = 1.
            ENDIF.

          ENDIF.

          zfit0026_wa-mont_mi       = p_edit-mont_moeda * zfit0026_wa-taxa.


        ELSE.
          zfit0026_wa-taxa          = p_edit-taxa.
          zfit0026_wa-mont_mi       = p_edit-mont_moeda * p_edit-taxa.
        ENDIF.

        zfit0026_wa-forma_pag     = p_edit-forma_pag.
        zfit0026_wa-uname         = sy-uname.
        zfit0026_wa-data_registro = sy-datum.
*          zfit0026_wa-observacao    = p_editor-line.

        CONCATENATE zfit0026_wa-vbeln zfit0026_wa-seq sy-datum(4) INTO obj_key.

        zfit0026_wa-obj_key = obj_key.

*          IF ( rbutton_acerto EQ 'X' ).
*            zfit0026_wa-razao_especial = 'G'.
*          ELSEIF ( rbutton_deposito EQ 'X' ).
*            zfit0026_wa-razao_especial = 'L'.
*          ENDIF.

        INSERT INTO zfit0026 VALUES zfit0026_wa.

        CLEAR: it_lanc_ver[], wa_lanc_ver.

        SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
               data_registro bukrs obj_key docnum  zterm doc_fatura data_pgto
               vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
               razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros
            FROM zfit0026
            INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
            WHERE vbeln EQ  p_edit-vbeln.

        IF it_lanc_ver[] IS NOT INITIAL.

          SELECT *
            FROM zib_contabil_chv
            INTO TABLE it_zib_contabil_chv
            FOR ALL ENTRIES IN it_lanc_ver
            WHERE obj_key EQ it_lanc_ver-obj_key
              AND NOT EXISTS ( SELECT *
                                 FROM bkpf
                                WHERE belnr EQ zib_contabil_chv~belnr
                                  AND bukrs EQ zib_contabil_chv~bukrs
                                  AND gjahr EQ zib_contabil_chv~gjahr
                                  AND stblg NE space ).

          SELECT *
            FROM zsdt0159
            INTO TABLE it_z0159
            FOR ALL ENTRIES IN it_lanc_ver
            WHERE obj_key EQ it_lanc_ver-obj_key.

        ENDIF.
* ---> S4 Migration - 04/07/2023 - FTM - Inicio
        SORT: it_lanc_ver BY vbeln.
* <--- S4 Migration - 04/07/2023 - FTM - Fim
        READ TABLE it_lanc_ver INTO wa_lanc_ver WITH KEY vbeln = p_edit-vbeln BINARY SEARCH.

        IF ( sy-subrc EQ 0 ).

          CLEAR: it_lanc[].

          wa_lanc-edit           = icon_change.
          wa_lanc-excluir        = icon_delete.

          IF wa_lanc_ver-ajuste NE 'X'.
            wa_lanc-gera           = icon_activity.
            wa_lanc-estor          = icon_reject.
          ELSE.
            CLEAR: wa_lanc-gera, wa_lanc-estor.
          ENDIF.

          wa_lanc-vbeln          = wa_lanc_ver-vbeln.
          wa_lanc-seq            = wa_lanc_ver-seq.
          wa_lanc-data_venc      = wa_lanc_ver-data_venc.
          wa_lanc-moeda          = wa_lanc_ver-moeda.
          wa_lanc-mont_moeda     = wa_lanc_ver-mont_moeda.
          wa_lanc-taxa           = wa_lanc_ver-taxa.
          wa_lanc-mont_mi        = wa_lanc_ver-mont_mi.
          wa_lanc-zterm          = wa_lanc_ver-zterm.

          wa_lanc-taxa           = wa_lanc_ver-taxa.

          wa_lanc-forma_pag      = wa_lanc_ver-forma_pag.
          wa_lanc-uname          = wa_lanc_ver-uname.
          wa_lanc-data_registro  = wa_lanc_ver-data_registro.

          wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
          wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
          wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
          wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.

          IF wa_lanc_ver-ajuste IS NOT INITIAL.
            wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_desc_mult.
            wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_desc_jros.
          ELSE.
            wa_lanc-vlr_multa_rbdo  = wa_lanc_ver-vlr_multa_rbdo.
            wa_lanc-vlr_juros_rbdo  = wa_lanc_ver-vlr_juros_rbdo.
          ENDIF.

          IF p_ins IS NOT INITIAL.

            READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

            IF sy-subrc = 0.
              READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
              IF sy-subrc EQ 0.
                "PAGAMENTO ANTECIPADO
                CASE wa_zsdt0040-meio_pago .
                  WHEN 'D' .
                    wa_lanc-pgto_ant = 'Deposito em Conta'.
                  WHEN 'A' .
                    wa_lanc-pgto_ant = 'Acerto'.
                  WHEN 'B' .
                    wa_lanc-pgto_ant = 'Boleto Bancário'.
                  WHEN ' ' .
                    wa_lanc-pgto_ant = 'Não Atencipado'.
                ENDCASE.
              ENDIF.
            ENDIF.
          ELSE.


            SELECT SINGLE *
            FROM zsdt0053 INTO @DATA(wa_zsdt0053)
            WHERE vbeln = @wa_lanc_ver-vbeln.

            "PAGAMENTO ANTECIPADO

            SELECT SINGLE *
              FROM zsdt0052 INTO @DATA(wa_zsdt0052)
              WHERE nro_sol_ov = @wa_zsdt0053-nro_sol_ov.

            CASE wa_zsdt0052-pgto_ant .

              WHEN 'X' .
                wa_lanc-pgto_ant = ' Com Boleto '.

              WHEN 'N' .
                wa_lanc-pgto_ant = ' Sem Boleto '.

              WHEN ' ' .
                wa_lanc-pgto_ant = ' Não Antecipado '.
            ENDCASE.

          ENDIF.

******Alteração CS2017000894 Início

          "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.

          READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
          IF sy-subrc IS INITIAL.
            wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
*                WA_LANC-STATUS_DOC  = 'P'.
          ELSE.
            READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
            IF sy-subrc IS INITIAL.
              wa_lanc-docnum         = wa_z0159-adiant.
            ELSE.
              CLEAR: wa_lanc-docnum.
            ENDIF.
          ENDIF.

*******Alteração CS2017000894 Fim
*          CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*          CLEAR: p_edit-observacao.
*          LOOP AT it_editor INTO p_editor.
*            wa_lanc-observacao = |{ wa_lanc-observacao } { p_editor-line }|.
*          ENDLOOP.

*              WA_LANC-OBSERVACAO     = p_editOR-LINE.

          CASE wa_lanc_ver-status.
            WHEN: 'P'.
              wa_lanc-status_doc = icon_generate.
            WHEN: 'E'.
              wa_lanc-status_doc = icon_led_red.
            WHEN: 'G'.
              wa_lanc-status_doc = icon_led_green.
            WHEN: 'X'.
              wa_lanc-status_doc = icon_booking_stop.
            WHEN: 'A'.
              wa_lanc-status_doc = icon_budget_update.
            WHEN OTHERS.
              wa_lanc-status_doc = icon_led_yellow.
          ENDCASE.

          IF wa_saida-auart IN r_devo_recu.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix + abs( wa_lanc-mont_moeda ).
            MULTIPLY wa_lanc-mont_moeda_parc BY -1.
          ELSE.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - wa_lanc-mont_moeda.
          ENDIF.

          APPEND wa_lanc TO it_lanc.

*          CALL METHOD grid_lancamento->refresh_table_display
*            EXPORTING
*              is_stable = wa_stable.
*
*          CLEAR: p_edit, edit.
*          MESSAGE s888(sabapdocu) WITH 'Lançamento cadastrado!'.
*              LEAVE TO SCREEN 0100.
        ENDIF.

*          CLEAR: p_edit.

*            LEAVE TO SCREEN 0100.

      ELSE.

        IF it_lanc[ vbeln = p_edit-vbeln
                    seq = p_edit-seq ]-docnum
          IS NOT INITIAL.

*          CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*          CLEAR: p_edit-observacao.
*          LOOP AT it_editor INTO p_editor.
*            p_edit-observacao = |{ p_edit-observacao } { p_editor-line }|.
*          ENDLOOP.

          UPDATE zfit0026 SET observacao     = p_edit-observacao
          WHERE vbeln        = p_edit-vbeln
            AND seq          = p_edit-seq.
          COMMIT WORK.

        ELSE.


          IF p_edit-ajuste IS NOT INITIAL AND p_edit-mont_moeda EQ 0.
            p_edit-vlr_desc_jros   = p_edit-vlr_juros_rbdo.
            p_edit-vlr_desc_mult   = p_edit-vlr_multa_rbdo.

            p_edit-vlr_juros_rbdo  = ' '.
            p_edit-vlr_multa_rbdo  = ' '.
            p_edit-vlr_juros_calc  = ' '.
            p_edit-vlr_multa_calc  = ' '.
          ELSE.
            p_edit-vlr_desc_jros   = ' '.
            p_edit-vlr_desc_mult   = ' '.
          ENDIF.

          IF ( p_edit-moeda EQ 'BRL' ) AND ( p_edit-taxa IS INITIAL ) .
            IF p_edit-ajuste IS INITIAL.
              p_edit-taxa = 1.
            ELSE.
              "BUSCA A TAXA
*                CLEAR: VL_UKURS.
              obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
              obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
              obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

              MOVE  p_edit-data_pgto TO vl_gdatu.
              obj_zcl_util_sd->set_data( vl_gdatu ).
              p_edit-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

              IF p_edit-taxa IS INITIAL.
                p_edit-taxa = 1.
              ENDIF.
            ENDIF.

            p_edit-mont_mi = p_edit-mont_moeda * p_edit-taxa.
          ELSE.
*              CLEAR: p_edit-mont_mi.
            p_edit-mont_mi = p_edit-mont_moeda * p_edit-taxa.
          ENDIF.

          CLEAR: razao_especial.

*          IF NOT ( rbutton_acerto IS INITIAL ).
*            razao_especial = 'G'.
*          ELSEIF NOT ( rbutton_deposito IS INITIAL ).
*            razao_especial = 'L'.
*          ENDIF.

*          CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*          CLEAR: p_edit-observacao.
*          LOOP AT it_editor INTO p_editor.
*            p_edit-observacao = |{ p_edit-observacao } { p_editor-line }|.
*          ENDLOOP.
*              READ TABLE IT_EDITOR INTO p_editOR INDEX 1.
*              p_edit-OBSERVACAO = p_editOR-LINE.

          IF p_edit-ajuste EQ 'X'.
            p_edit-status = 'A'.
          ELSE.
            p_edit-status = ' '.
          ENDIF.

          UPDATE zfit0026 SET data_venc         = p_edit-data_venc
                              mont_moeda        = p_edit-mont_moeda
                              taxa              = p_edit-taxa
                              mont_mi           = p_edit-mont_mi
                              forma_pag         = p_edit-forma_pag
                              status            = p_edit-status
                              data_registro     = sy-datum
                              razao_especial    = razao_especial
                              observacao        = p_edit-observacao
                              zterm             = p_edit-zterm
                              ajuste            = p_edit-ajuste
                              data_pgto         = p_edit-data_pgto
                              mont_rbdo         = p_edit-mont_rbdo
                              vlr_juros_rbdo    = p_edit-vlr_juros_rbdo
                              vlr_multa_rbdo    = p_edit-vlr_multa_rbdo
                              vlr_juros_calc    = p_edit-vlr_juros_calc
                              vlr_multa_calc    = p_edit-vlr_multa_calc
                              vlr_desc_jros     = p_edit-vlr_desc_jros
                              vlr_desc_mult     = p_edit-vlr_desc_mult
                              rec_vlr_total     = p_edit-rec_vlr_total
                              doc_fatura        = p_edit-doc_fatura
          WHERE vbeln        = p_edit-vbeln
            AND seq          = p_edit-seq.
          COMMIT WORK.

        ENDIF.

        IF sy-subrc IS INITIAL.

          MESSAGE s888(sabapdocu) WITH 'Registro atualizado.'.

          CLEAR: it_lanc[],
                 wa_lanc,
                 it_lanc_ver[],
                 wa_lanc_ver.

          SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
                 data_registro bukrs obj_key docnum   zterm doc_fatura data_pgto
                 vlr_multa_rbdo  vlr_juros_rbdo mont_rbdo vlr_multa_calc
                 razao_especial observacao ajuste vlr_juros_calc rec_vlr_total vlr_desc_mult vlr_desc_jros num_comp_adiant
           FROM zfit0026
           INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
           WHERE vbeln EQ  p_edit-vbeln.

          SORT: it_lanc_ver BY seq.
          IF ( sy-subrc EQ 0 ).

            IF it_lanc_ver[] IS NOT INITIAL.

              SELECT *
                FROM zib_contabil_chv
                INTO TABLE it_zib_contabil_chv
                FOR ALL ENTRIES IN it_lanc_ver
                WHERE obj_key EQ it_lanc_ver-obj_key
                  AND NOT EXISTS ( SELECT *
                                     FROM bkpf
                                    WHERE belnr EQ zib_contabil_chv~belnr
                                      AND bukrs EQ zib_contabil_chv~bukrs
                                      AND gjahr EQ zib_contabil_chv~gjahr
                                      AND stblg NE space ).

              SELECT *
               FROM zsdt0159
               INTO TABLE it_z0159
               FOR ALL ENTRIES IN it_lanc_ver
               WHERE obj_key EQ it_lanc_ver-obj_key.

            ENDIF.

            CLEAR: soma.

            LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ p_edit-vbeln.

              wa_lanc-edit           = icon_change.
              wa_lanc-excluir        = icon_delete.

              IF wa_lanc_ver-ajuste NE 'X'.
                wa_lanc-gera           = icon_activity.
                wa_lanc-estor          = icon_reject.
              ELSE.
                CLEAR:  wa_lanc-gera, wa_lanc-estor .
              ENDIF.

              wa_lanc-vbeln           = wa_lanc_ver-vbeln.
              wa_lanc-seq             = wa_lanc_ver-seq.
              wa_lanc-data_venc       = wa_lanc_ver-data_venc.
              wa_lanc-moeda           = wa_lanc_ver-moeda.
              wa_lanc-mont_moeda      = wa_lanc_ver-mont_moeda.
              wa_lanc-taxa            = wa_lanc_ver-taxa.
              wa_lanc-mont_mi         = wa_lanc_ver-mont_mi.
              wa_lanc-forma_pag       = wa_lanc_ver-forma_pag.
              wa_lanc-uname           = wa_lanc_ver-uname.
              wa_lanc-data_registro   = wa_lanc_ver-data_registro.
              wa_lanc-ajuste          = wa_lanc_ver-ajuste.
              wa_lanc-data_pgto       = wa_lanc_ver-data_pgto.
*                  WA_LANC-VLR_MULTA_RBDO  = WA_LANC_VER-VLR_MULTA_RBDO.
*                  WA_LANC-VLR_JUROS_RBDO  = WA_LANC_VER-VLR_JUROS_RBDO.
*                  WA_LANC-VLR_MULTA_CALC  = WA_LANC_VER-VLR_MULTA_CALC.
*                  WA_LANC-VLR_JUROS_CALC  = WA_LANC_VER-VLR_JUROS_CALC.
              wa_lanc-mont_rbdo       = wa_lanc_ver-mont_rbdo.
              wa_lanc-rec_vlr_total   = wa_lanc_ver-rec_vlr_total.
              wa_lanc-doc_fatura      = wa_lanc_ver-doc_fatura.
              wa_lanc-num_comp_adiant = wa_lanc_ver-num_comp_adiant.

              IF p_edit-ajuste IS NOT INITIAL AND p_edit-mont_moeda EQ 0.
                zfit0026_wa-vlr_desc_jros   = p_edit-vlr_juros_rbdo.
                zfit0026_wa-vlr_desc_mult   = p_edit-vlr_multa_rbdo.
              ELSE.
                zfit0026_wa-vlr_juros_rbdo   = p_edit-vlr_juros_rbdo.
                zfit0026_wa-vlr_multa_rbdo   = p_edit-vlr_multa_rbdo.
              ENDIF.

              IF p_edit-ajuste IS INITIAL AND p_edit-mont_moeda NE 0. "
                zfit0026_wa-vlr_juros_calc   = p_edit-vlr_juros_calc.
                zfit0026_wa-vlr_multa_calc   = p_edit-vlr_multa_calc.
              ELSE.
                zfit0026_wa-vlr_juros_calc   = ' '.
                zfit0026_wa-vlr_multa_calc   = ' '.
              ENDIF.

              IF p_ins IS NOT INITIAL.

                READ TABLE it_zsdt0041 INTO wa_zsdt0041 WITH KEY vbeln = wa_lanc_ver-vbeln.

                IF sy-subrc = 0.
                  READ TABLE it_zsdt0040 INTO wa_zsdt0040 WITH KEY doc_simulacao = wa_zsdt0041-doc_simulacao.
                  IF sy-subrc EQ 0.
                    "PAGAMENTO ANTECIPADO
                    CASE wa_zsdt0040-meio_pago .
                      WHEN 'D' .
                        wa_lanc-pgto_ant = 'Deposito em Conta'.
                      WHEN 'A' .
                        wa_lanc-pgto_ant = 'Acerto'.
                      WHEN 'B' .
                        wa_lanc-pgto_ant = 'Boleto Bancário'.
                      WHEN ' ' .
                        wa_lanc-pgto_ant = 'Não Atencipado'.
                    ENDCASE.
                  ENDIF.
                ENDIF.
              ELSE.


                CLEAR: wa_zsdt0053.
                SELECT SINGLE *
                FROM zsdt0053 INTO wa_zsdt0053
                WHERE vbeln = wa_lanc_ver-vbeln.

                "PAGAMENTO ANTECIPADO

                CLEAR: wa_zsdt0052.
                SELECT SINGLE *
                  FROM zsdt0052 INTO wa_zsdt0052
                  WHERE nro_sol_ov = wa_zsdt0053-nro_sol_ov.

                CASE wa_zsdt0052-pgto_ant .

                  WHEN 'X' .
                    wa_lanc-pgto_ant = ' Com Boleto '.

                  WHEN 'N' .
                    wa_lanc-pgto_ant = ' Sem Boleto '.

                  WHEN ' ' .
                    wa_lanc-pgto_ant = ' Não Antecipado '.
                ENDCASE.

              ENDIF.

******Alteração CS2017000894 Início

              "WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.

              READ TABLE it_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = wa_lanc_ver-obj_key.
              IF sy-subrc IS INITIAL.
                wa_lanc-docnum       = wa_zib_contabil_chv-belnr.
*                    WA_LANC-STATUS_DOC  = 'P'.
              ELSE.
                READ TABLE it_z0159 INTO wa_z0159 WITH KEY obj_key = wa_lanc_ver-obj_key.
                IF sy-subrc IS INITIAL.
                  wa_lanc-docnum         = wa_z0159-adiant.
                ELSE.
                  CLEAR: wa_lanc-docnum.
                ENDIF.
              ENDIF.

******Alteração CS2017000894 Fim

              wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
              wa_lanc-razao_especial = wa_lanc_ver-razao_especial.
              wa_lanc-observacao     = wa_lanc_ver-observacao.
              wa_lanc-zterm          = wa_lanc_ver-zterm.

              CASE wa_lanc_ver-status.
                WHEN: 'P'.
                  wa_lanc-status_doc = icon_generate.
                WHEN: 'E'.
                  wa_lanc-status_doc = icon_led_red.
                WHEN: 'G'.
                  wa_lanc-status_doc = icon_led_green.
                WHEN: 'X'.
                  wa_lanc-status_doc = icon_booking_stop.
                WHEN: 'A'.
                  wa_lanc-status_doc = icon_budget_update.
                WHEN OTHERS.
                  wa_lanc-status_doc = icon_led_yellow.
              ENDCASE.

*              CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*              CLEAR: p_edit-observacao.
*              LOOP AT it_editor INTO p_editor.
*                wa_lanc-observacao = |{ wa_lanc-observacao } { p_editor-line }|.
*              ENDLOOP.


              wa_lanc-mont_moeda_fix = wa_saida-netwr.

              soma      = soma + wa_lanc_ver-mont_moeda.

              APPEND wa_lanc TO it_lanc.
            ENDLOOP.

            IF wa_saida-auart IN r_devo_recu.
              wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
              MULTIPLY wa_lanc-mont_moeda_parc BY -1.
            ELSE.
              wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
            ENDIF.

*            CALL METHOD grid_lancamento->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.


*              CLEAR: p_edit, it_edit[].
*                FREE IT_EDITOR.

*                LEAVE TO SCREEN 0100.

          ENDIF.
        ENDIF.
      ENDIF.

      CLEAR: edit.

    ELSE.


      IF ( p_edit-ajuste NE 'X' ) AND ( p_edit-mont_moeda > wa_lanc-mont_moeda_fix ).
*          CLEAR: p_edit.
*          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH texto_01 texto_02.
      ELSEIF ( p_edit-ajuste <> 'X' ) AND  ( p_edit-mont_moeda > wa_lanc-mont_moeda_parc ) AND ( cont NE 0 ).
*          CLEAR: p_edit.
*          MESSAGE s888(sabapdocu) DISPLAY LIKE 'E' WITH texto_03 texto_04.
      ELSE.

        CLEAR: obj_key.

        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr = '01'
            object      = 'ZID_LANC'
          IMPORTING
            number      = p_zid.

        SELECT vbeln seq
          FROM zfit0026
          INTO TABLE it_cont_seq
        WHERE vbeln EQ p_edit-vbeln
          ORDER BY seq DESCENDING.

        READ TABLE it_cont_seq INTO wa_cont_seq INDEX 1.

        zfit0026_wa-zid_lanc      = p_zid.
        zfit0026_wa-seq           = wa_cont_seq-seq + 1.
        zfit0026_wa-vbeln         = p_edit-vbeln.
        zfit0026_wa-data_venc     = p_edit-data_venc.
        zfit0026_wa-bukrs         = p_edit-bukrs.
        zfit0026_wa-zterm         = p_edit-zterm.

        zfit0026_wa-moeda         = p_edit-moeda.

        zfit0026_wa-mont_moeda    = p_edit-mont_moeda.
        zfit0026_wa-ajuste        = p_edit-ajuste.

        IF ( zfit0026_wa-moeda EQ 'BRL' ).
          IF p_edit-ajuste IS INITIAL.
            zfit0026_wa-taxa          = 1.
          ELSE.
            "BUSCA A TAXA
*                CLEAR: VL_UKURS.
            obj_zcl_util_sd->set_kurst( EXPORTING i_kurst = 'G' ).
            obj_zcl_util_sd->set_waerk( EXPORTING i_waerk = 'USD' ).
            obj_zcl_util_sd->set_tcurr( EXPORTING i_tcurr = 'BRL' ).

            MOVE  p_edit-data_pgto TO vl_gdatu.
            obj_zcl_util_sd->set_data( vl_gdatu ).
            zfit0026_wa-taxa = obj_zcl_util_sd->taxa_cambio_dia( ).

            IF ( zfit0026_wa-taxa IS INITIAL ) OR ( zfit0026_wa-moeda EQ 'BRL' ). "alteracao feita por Alexandre Rimini 04.04.2023 - antes era  IF zfit0026_wa-taxa IS INITIAL.
              zfit0026_wa-taxa = 1.
            ENDIF.
          ENDIF.
          zfit0026_wa-mont_mi       = p_edit-mont_moeda * zfit0026_wa-taxa.
        ELSE.
          zfit0026_wa-taxa          = p_edit-taxa.
          zfit0026_wa-mont_mi       = p_edit-mont_moeda * zfit0026_wa-taxa.
        ENDIF.

        IF ( rbutton_acerto EQ 'X' ).
          zfit0026_wa-razao_especial = 'G'.
        ELSEIF ( rbutton_deposito EQ 'X' ).
          zfit0026_wa-razao_especial = 'L'.
        ENDIF.

        zfit0026_wa-forma_pag     = p_edit-forma_pag.
        IF zfit0026_wa-ajuste EQ 'X'.
          zfit0026_wa-status        = 'A'.
        ELSE.
          zfit0026_wa-status        = space.
        ENDIF.
        zfit0026_wa-uname         = sy-uname.
        zfit0026_wa-data_registro = sy-datum.
        zfit0026_wa-observacao    = p_edit-observacao.

        zfit0026_wa-data_pgto        = p_edit-data_pgto.
        zfit0026_wa-mont_rbdo        = p_edit-mont_rbdo.
*            ZFIT0026_WA-VLR_JUROS_RBDO   = p_edit-VLR_JUROS_RBDO.
*            ZFIT0026_WA-VLR_MULTA_RBDO   = p_edit-VLR_MULTA_RBDO.
*            ZFIT0026_WA-VLR_JUROS_CALC   = p_edit-VLR_JUROS_CALC.
*            ZFIT0026_WA-VLR_MULTA_CALC   = p_edit-VLR_MULTA_CALC.
        zfit0026_wa-doc_fatura       = p_edit-doc_fatura.
        zfit0026_wa-rec_vlr_total    = p_edit-rec_vlr_total.

        IF p_edit-ajuste IS NOT INITIAL AND p_edit-mont_moeda EQ 0.
          zfit0026_wa-vlr_desc_jros   = p_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_desc_mult   = p_edit-vlr_multa_rbdo.
        ELSE.
          zfit0026_wa-vlr_juros_rbdo   = p_edit-vlr_juros_rbdo.
          zfit0026_wa-vlr_multa_rbdo   = p_edit-vlr_multa_rbdo.
        ENDIF.

        IF p_edit-ajuste IS INITIAL AND p_edit-mont_moeda NE 0. "
          zfit0026_wa-vlr_juros_calc   = p_edit-vlr_juros_calc.
          zfit0026_wa-vlr_multa_calc   = p_edit-vlr_multa_calc.
        ELSE.
          zfit0026_wa-vlr_juros_calc   = ' '.
          zfit0026_wa-vlr_multa_calc   = ' '.
        ENDIF.

        CONCATENATE zfit0026_wa-vbeln zfit0026_wa-seq sy-datum(4) INTO obj_key.


******Alteração CS2017000894 Fim
*          FREE it_editor.
*          CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*          CLEAR: zfit0026_wa-observacao.
*          LOOP AT it_editor INTO p_editor.
*            zfit0026_wa-observacao = |{ zfit0026_wa-observacao } { p_editor-line }|.
*          ENDLOOP.

        zfit0026_wa-obj_key = obj_key.

        INSERT INTO zfit0026 VALUES zfit0026_wa.

        CLEAR: it_lanc[],
               wa_lanc,
               it_lanc_ver[],
               wa_lanc_ver.


        SELECT vbeln seq data_venc moeda mont_moeda taxa mont_mi forma_pag status uname
               data_registro bukrs obj_key docnum zterm doc_fatura data_pgto
               vlr_multa_rbdo vlr_juros_rbdo mont_rbdo vlr_multa_calc vlr_juros_calc
               razao_especial observacao ajuste rec_vlr_total vlr_desc_mult vlr_desc_jros
         FROM zfit0026
         INTO CORRESPONDING FIELDS OF TABLE it_lanc_ver
      WHERE vbeln EQ  p_edit-vbeln.

        SORT: it_lanc_ver BY seq.

        IF ( sy-subrc EQ 0 ).

          CLEAR: soma.

          LOOP AT it_lanc_ver INTO wa_lanc_ver WHERE vbeln EQ p_edit-vbeln.

            "                MOVE-CORRESPONDING WA_LANC TO WA_LANC_VER.
            MOVE-CORRESPONDING wa_lanc_ver TO wa_lanc.

            wa_lanc-edit           = icon_change.
            wa_lanc-excluir        = icon_delete.
            IF wa_lanc_ver-ajuste NE 'X'.
              wa_lanc-gera           = icon_activity.
              wa_lanc-estor          = icon_reject.
            ELSE.
              CLEAR: wa_lanc-gera, wa_lanc-estor.
            ENDIF.
*                WA_LANC-VBELN          = WA_LANC_VER-VBELN.
*                WA_LANC-SEQ            = WA_LANC_VER-SEQ.
*                WA_LANC-DATA_VENC      = WA_LANC_VER-DATA_VENC.
*                WA_LANC-MOEDA          = WA_LANC_VER-MOEDA.
*                WA_LANC-MONT_MOEDA     = WA_LANC_VER-MONT_MOEDA.
*                WA_LANC-TAXA           = WA_LANC_VER-TAXA.
*                WA_LANC-MONT_MI        = WA_LANC_VER-MONT_MI.
*                WA_LANC-FORMA_PAG      = WA_LANC_VER-FORMA_PAG.
*                WA_LANC-UNAME          = WA_LANC_VER-UNAME.
*                WA_LANC-DATA_REGISTRO  = WA_LANC_VER-DATA_REGISTRO.
*                WA_LANC-DOCNUM         = WA_LANC_VER-DOCNUM.
            wa_lanc-bukrs_vf       = wa_lanc_ver-bukrs.
            wa_lanc-ajuste         = wa_lanc_ver-ajuste.
*                WA_LANC-RAZAO_ESPECIAL = WA_LANC_VER-RAZAO_ESPECIAL.
*                WA_LANC-OBSERVACAO     = WA_LANC_VER-OBSERVACAO.

            CASE wa_lanc-razao_especial.
              WHEN: 'G'.
                wa_lanc-razao = 'Acerto'.
              WHEN: 'L'.
                wa_lanc-razao = 'Depósito'.
            ENDCASE.

            CASE wa_lanc_ver-status.
              WHEN: 'P'.
                wa_lanc-status_doc = icon_generate.
              WHEN: 'E'.
                wa_lanc-status_doc = icon_led_red.
              WHEN: 'G'.
                wa_lanc-status_doc = icon_led_green.
              WHEN: 'X'.
                wa_lanc-status_doc = icon_booking_stop.
              WHEN: 'A'.
                wa_lanc-status_doc = icon_budget_update.  " ICON_MONEY   /  ICON_BUDGET_UPDATE / ICON_INSERT_RELATION /  ICON_PRICE /  ICON_VARIABLE  / ICON_TE_COSTS_ASSIGN
              WHEN OTHERS.
                wa_lanc-status_doc = icon_led_yellow.
            ENDCASE.

            wa_lanc-mont_moeda_fix = wa_saida-netwr.

            wa_lanc-observacao = wa_lanc_ver-observacao.

            soma = soma + wa_lanc_ver-mont_moeda.

            APPEND wa_lanc TO it_lanc.

          ENDLOOP.

          IF wa_saida-auart IN r_devo_recu.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - abs( soma ).
            MULTIPLY wa_lanc-mont_moeda_parc BY -1.
          ELSE.
            wa_lanc-mont_moeda_parc = wa_lanc-mont_moeda_fix - soma.
          ENDIF.

*            CALL METHOD grid_lancamento->refresh_table_display
*              EXPORTING
*                is_stable = wa_stable.

*            CLEAR: p_edit, it_edit[].

*              LEAVE TO SCREEN 0100.

        ENDIF.
      ENDIF.
    ENDIF.
  ENDIF.
*  ENDIF.
*  ENDIF.

  " 03.04.2023 - RAMON - 97568 -->
  IF p_erro IS INITIAL.

    DATA lt_zsdt0315 TYPE TABLE OF zsdt0315. " ---> Chamado MG-5804 - 25.07.2023 - LM

    CALL FUNCTION 'ZSDMF_GRAVA_REG_ZSDT0315'
      EXPORTING
        iv_vbeln             = zfit0026_wa-vbeln
*       IV_VBELV             =
        iv_waers             = zfit0026_wa-moeda
        iv_valor_ov          = wa_lanc-mont_moeda_fix
        iv_liqui             = wa_lanc-mont_moeda
*       IV_VLR_DESM          =
        iv_commit            = 'X'
*      TABLES
*       et_zsdt0315          =
* ---> Chamado MG-5804 - 25.07.2023 - LM
      TABLES
        et_zsdt0315          = lt_zsdt0315
* <--- Chamado MG-5804 - 25.07.2023 - LM
      EXCEPTIONS
        ov_100_liquidada     = 1
        vlr_desm_maior_ov    = 2
        desm_e_liqui         = 3
        vlr_liqui_maior_ov   = 4
        informar_ov_desmem   = 5
        vlr_ov_desatualizado = 6
        ov_nova_existente    = 7
        OTHERS               = 8.

    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.

  ENDIF.

  " 03.04.2023 - RAMON - 97568 --<


  IF p_erro IS INITIAL.
*    CALL METHOD editor->get_text_as_stream( IMPORTING text = it_editor ).
*    CLEAR: p_edit-observacao.
*    LOOP AT it_editor INTO p_editor.
*      p_edit-observacao = |{ p_edit-observacao } { p_editor-line }|.
*    ENDLOOP.
*  READ TABLE IT_EDITOR INTO p_editOR INDEX 1.
*  p_edit-OBSERVACAO = p_editOR-LINE.

    IF NOT p_edit-observacao IS INITIAL.
      UPDATE zfit0026 SET   observacao     = p_edit-observacao
                      WHERE vbeln        = p_edit-vbeln
                        AND seq          = p_edit-seq.

    ENDIF.


*    FREE: it_editor.
  ENDIF.

  IF obj_key IS NOT INITIAL.

    LOOP AT p_026 ASSIGNING FIELD-SYMBOL(<fs_026>).

      <fs_026>-obj_key_v = obj_key.

    ENDLOOP.
    MODIFY zfit0026 FROM TABLE p_026[].
    IF sy-subrc IS INITIAL.
      COMMIT WORK.
    ENDIF.
  ENDIF.
ENDFORM.
