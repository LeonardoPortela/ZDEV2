*&---------------------------------------------------------------------*
*& Report ZSDR0234
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zsdr0234.

TYPES: BEGIN OF ty_saida,
         filial         TYPE zlest0061-werks,
         safra          TYPE zlest0061-safra,
         cod_material   TYPE zlest0061-cod_material,
         tp_class       TYPE zlest0061-tp_class,
         nr_dco         TYPE zlest0061-nr_dco,
         peso_vinculado TYPE zlest0061-peso_vinculado,
         vlr_brl        TYPE zlest0061-vlr_brl,
         vlr_usd        TYPE zlest0061-vlr_usd,
         dt_fatura      TYPE zlest0061-dt_fatura,
         docnum         TYPE zlest0061-docnum,
         nf_saida       TYPE j_1bnfdoc-nfenum,
         series         TYPE j_1bnfdoc-series,
         doc_znfw       TYPE zsdt0225-doc_znfw,
         docnum_entrada TYPE zsdt0225-doc_znfw,
         id_seq         TYPE zsdt0225-id_seq,
         operacao       TYPE zsdt0229-operacao,
         matnr          TYPE zsdt0225-matnr_ov,
         bukrs          TYPE zsdt0225-bukrs,
         doc_contabil   TYPE bkpf-belnr,
         b_bukrs        TYPE bkpf-bukrs,
         b_gjahr        TYPE bkpf-gjahr,
         werks_serv     TYPE zsdt0225-werks_serv,
         auart          TYPE auart,
       END OF ty_saida.


DATA: gt_status     TYPE zde_btcstatus_t,
      lt_dados_exec TYPE zsdt_compensacao_automatica,
      it_cskb       TYPE TABLE OF cskb,
      p_parvw       TYPE zfiwrt0008-parvw,
      p_parid       TYPE zfiwrt0008-parid,
      wa_indcoper   TYPE zfiwrt0006-indcoper,
      it_j_1bnfdoc  TYPE TABLE OF j_1bnfdoc,
      wa_j_1bnfdoc  TYPE j_1bnfdoc,
      lw_saida      TYPE ty_saida,
      lv_seq_lcto   TYPE zfiwrt0008-seq_lcto.

DATA: lv_jobcount TYPE tbtcjob-jobcount,
      lv_jobname  TYPE tbtcjob-jobname.


PARAMETERS: p_bukrs TYPE bukrs NO-DISPLAY.

START-OF-SELECTION.

  APPEND 'R' TO gt_status.

*---------------------------------------------
* Se tem Job ativo, abandona
*---------------------------------------------
  IF sy-batch = abap_true.
    TRY .
        zcl_job=>get_job_programa_execucao(
          EXPORTING
            i_progname   = sy-cprog
            i_sdldate    = sy-datum
            i_status     = gt_status
          IMPORTING
            e_quantidade = DATA(e_qtd) ).
      CATCH zcx_job.
    ENDTRY.

    IF e_qtd > 2.
      EXIT.
    ENDIF.
  ENDIF.

  PERFORM f_monta_dados.

FORM f_nota_remessa USING t_saida TYPE ty_saida
                    CHANGING t_seq_lcto TYPE zfiwrt0008-seq_lcto.

  DATA: wl_0008 TYPE zfiwrt0008,
        wl_0009 TYPE zfiwrt0009,
        wl_0023 TYPE zfiwrt0023.

* ---> S4 Migration - 18/07/2023 - CA
  DATA: lt_returns         TYPE TABLE OF bapiret2,
        ls_coeldes         TYPE bapi1030_ceoutputlist,
        ls_cskb            LIKE LINE OF it_cskb,
        lv_controllingarea TYPE  bapi1030_gen-co_area,
        lv_costelement     TYPE  bapi1030_gen-cost_elem,
        lv_keydate         TYPE  bapi1030_gen-some_date.
* <--- S4 Migration - 18/07/2023 - CA

  CLEAR:  p_parid, p_parvw.


  SELECT SINGLE * FROM zsdt0229 INTO @DATA(w229)
    WHERE operacao EQ @t_saida-operacao.

  IF sy-subrc <> 0.
    MESSAGE 'Não Existe operação Nota Writer Cadastrada!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  SELECT SINGLE * FROM zfiwrt0001 INTO @DATA(wl_0001)
    WHERE operacao EQ @t_saida-operacao.

  IF sy-subrc <> 0.
    MESSAGE 'Não Existe operação Nota Writer Cadastrada!' TYPE 'S' DISPLAY LIKE 'E'.
    EXIT.
  ENDIF.

  p_parid = |{ t_saida-werks_serv ALPHA = IN }|.
  p_parvw   = wl_0001-parvw.


  SELECT * FROM zfiwrt0006 INTO TABLE @DATA(tl_0006)
    WHERE operacao EQ @t_saida-operacao.

  SELECT SINGLE *  FROM t001w  INTO @DATA(wa_t001w) WHERE werks EQ @t_saida-filial.

  IF wl_0001-parvw EQ 'AG'.
    SELECT SINGLE * FROM kna1 INTO @DATA(wa_kna1)
      WHERE kunnr EQ @p_parid.

  ELSEIF wl_0001-parvw EQ 'BR' OR wl_0001-parvw EQ 'LF'.
    SELECT SINGLE * FROM lfa1 INTO @DATA(wa_lfa1)
      WHERE lifnr EQ @p_parid.
  ENDIF.


  IF wl_0001-parvw EQ 'AG'.
    IF wa_kna1-regio EQ wa_t001w-regio.
      wa_indcoper = 'D'.
    ELSE.
      wa_indcoper = 'F'.
    ENDIF.
  ELSEIF wl_0001-parvw EQ 'BR' OR wl_0001-parvw EQ 'LF'.
    IF wa_lfa1-regio EQ wa_t001w-regio.
      wa_indcoper = 'D'.
    ELSE.
      wa_indcoper = 'F'.
    ENDIF.
  ENDIF.

  CLEAR wl_0008.

  READ TABLE tl_0006 INTO DATA(wl_0006) WITH KEY indcoper = wa_indcoper.

  SELECT SINGLE * FROM j_1bbranch INTO @DATA(w1bbranch)
    WHERE branch EQ @t_saida-filial.

  SELECT  *
   FROM zfiwrt0003 INTO TABLE @DATA(it_zfiwrt0003)
     WHERE operacao EQ @t_saida-operacao.

  SELECT SINGLE * FROM tka02 INTO @DATA(wl_tka02)
    WHERE bukrs EQ @w1bbranch-bukrs.

  LOOP AT it_zfiwrt0003 INTO DATA(ls_0003).

    lv_controllingarea  = wl_tka02-kokrs.
    lv_costelement      = ls_0003-hkont.
    lv_keydate          = sy-datum.

    CLEAR: lt_returns[], ls_coeldes.

    CALL FUNCTION 'K_COSTELEM_BAPI_GETDETAIL'
      EXPORTING
        controllingarea   = lv_controllingarea
        costelement       = lv_costelement
        keydate           = lv_keydate
      IMPORTING
        costelementdetail = ls_coeldes
      TABLES
        return            = lt_returns.

    READ TABLE lt_returns TRANSPORTING NO FIELDS WITH KEY type = 'E'.
    IF sy-subrc <> 0.
      ls_cskb-kokrs = wl_tka02-kokrs.
      ls_cskb-kstar = ls_0003-hkont.
      ls_cskb-katyp = ls_coeldes-celem_category.

      APPEND ls_cskb TO it_cskb.
      CLEAR ls_cskb.
    ENDIF.

    CLEAR: ls_0003.
  ENDLOOP.

  IF it_cskb[] IS NOT INITIAL.
*  IF SY-SUBRC = 0.
* <--- S4 Migration - 18/07/2023 - CA

*** Inicio - Rubenilson Pereira - 29.07.25 #181046
    SELECT SINGLE matkl
      FROM mara
      INTO @DATA(lv_matkl)
      WHERE matnr = @t_saida-matnr.
    IF sy-subrc IS INITIAL.
*** Fim - Rubenilson Pereira - 29.07.25 #181046
      SELECT SINGLE * FROM zcot0011 INTO  @DATA(wl_zcot0011)
     WHERE  bukrs EQ @w1bbranch-bukrs
     AND    werks EQ @t_saida-filial
     AND    matkl EQ @lv_matkl." Rubenilson Pereira - 29.07.25 #181046
    ENDIF.

    SELECT SINGLE * FROM csks INTO @DATA(wcsks)
      WHERE kostl EQ @wl_zcot0011-kostl
       AND  bukrs EQ @w1bbranch-bukrs
       AND  gsber EQ @t_saida-filial.

    IF  wcsks-bkzkp EQ 'X'.
      MESSAGE 'Centro de custo bloqueado p/ Lançamentos  !' TYPE 'S' DISPLAY LIKE 'E'.
      EXIT.
    ELSE.
      IF wl_zcot0011 IS NOT INITIAL.
        wl_0023-perc   = '100'.
        wl_0023-kostl  = wl_zcot0011-kostl.
      ELSE.
        MESSAGE 'Falta parâmetro na transação ZCO0029. Solicite Depto. Orçamentos e Custos.' TYPE 'S' DISPLAY LIKE 'E'.
        EXIT.
      ENDIF.
    ENDIF.
  ENDIF.

  READ TABLE it_j_1bnfdoc INTO wa_j_1bnfdoc WITH KEY docnum = t_saida-docnum
  BINARY SEARCH.

  wl_0008-operacao          =  t_saida-operacao.
  wl_0008-bukrs             =  w1bbranch-bukrs.
  wl_0008-branch            =  t_saida-filial.
  wl_0008-nfenum            =  t_saida-nf_saida.
  wl_0008-series            =  t_saida-series.
  wl_0008-ch_referencia     =  t_saida-id_seq.
  wl_0008-parid             =  p_parid.
  wl_0008-parvw             =  p_parvw.
  wl_0008-nftype            =  wl_0001-nftype.
  wl_0008-ctrl_zrfl         =  wl_0001-ctrl_zrfl.
  wl_0008-zpesagem          =  wl_0001-zpesagem.
  wl_0008-dias              =  wl_0001-dias.
  wl_0008-retorno           =  wl_0001-retorno.
  wl_0008-energia           =  wl_0001-energia.
  wl_0008-servico           =  wl_0001-servico.
  wl_0008-complemento       =  wl_0001-complemento.
  wl_0008-referencia        =  wl_0001-referencia.
  wl_0008-budat             =  t_saida-dt_fatura.
  wl_0008-bldat             =  t_saida-dt_fatura.
  wl_0008-usuario_ult_mod   =  sy-uname.
  wl_0008-dt_ult_mod        =  sy-datum.
  wl_0008-hr_ult_mod        =  sy-uzeit.
  wl_0008-inco1             =  'SRV'.
  wl_0008-inco2             =  'Serviços'.
  wl_0008-cfop              =  wl_0006-cfop.
  wl_0008-taxlw1            =  wl_0006-taxlw1.
  wl_0008-taxlw2            =  wl_0006-taxlw2.
  wl_0008-taxlw4            =  wl_0006-taxlw4.
  wl_0008-taxlw5            =  wl_0006-taxlw5.
  wl_0008-opertyp           =  wl_0006-opertyp.
  wl_0008-taxcode           =  wl_0006-taxcode.
  wl_0008-zlsch             = 'U'.
  wl_0008-tcode_org         = sy-tcode.
  wl_0008-not_check_xml     = abap_true.

  PERFORM forma_pgto USING wa_j_1bnfdoc-zterm
                           wa_j_1bnfdoc-docdat   CHANGING   wl_0008-zfbdt.

  CLEAR wl_0009.

  SELECT SINGLE *
    FROM mara INTO @DATA(wl_mara)
   WHERE matnr EQ @t_saida-matnr.

  SELECT SINGLE * FROM marc INTO @DATA(wa_marc)
    WHERE matnr EQ @t_saida-matnr.

  wl_0009-itmnum  =  10.
  wl_0009-matnr   =  t_saida-matnr.
  wl_0009-bwkey   =  t_saida-filial.
  wl_0009-cfop    =  wl_0006-cfop.
  wl_0009-menge   =  '1'.

  IF  wl_mara-mtart = 'ZDIE'.
    wl_0009-meins   = wl_mara-meins.
  ELSE.
    wl_0009-meins   =  wa_marc-ausme.
  ENDIF.

  wl_0009-netpr   =  t_saida-vlr_brl.

  wl_0009-netwr = CONV #( t_saida-vlr_brl ).

  wl_0009-itmtyp  =  wl_0001-itmtyp.


  TRY.
      zcl_nf_writer=>zif_nf_writer~get_instance( )->novo_lancamento( )->set_cabecalho( i_cabecalho =  wl_0008 )->add_item( i_item = wl_0009 )->set_rateio( i_rateio = wl_0023 ).

      zcl_nf_writer=>zif_nf_writer~get_instance( )->gravar_documento( IMPORTING e_seq_lcto = t_seq_lcto ).

      IF t_seq_lcto IS NOT INITIAL.

        MESSAGE |Lançamento { t_seq_lcto } gerado com sucesso!| TYPE 'S'.

        UPDATE zsdt0225 SET doc_znfw = t_seq_lcto
        WHERE id_seq EQ t_saida-id_seq
          and docnum eq t_saida-docnum.

      ELSE.
        MESSAGE |Houve um erro ao gravar o lançamento!| TYPE 'S'.
      ENDIF.

    CATCH zcx_nf_writer INTO DATA(zcx_nf_writer).
      zcx_nf_writer->zif_error~published_erro( i_msgty = 'S' i_msgty_display = 'E' ).
  ENDTRY.

ENDFORM.

FORM forma_pgto USING i_zterm
                      i_dt_doc TYPE j_1bnfdoc-docdat CHANGING r_dt_vencimento TYPE zfiwrt0008-zfbdt.


  DATA: tag1          TYPE bseg-zbd1t, " help variables
        tag2          TYPE bseg-zbd2t, " for due date
        tag3          TYPE bseg-zbd3t,
        help_due_date TYPE bseg-zfbdt.

  SELECT SINGLE * INTO @DATA(wa_t052)
    FROM t052
   WHERE zterm EQ @i_zterm.

  IF sy-subrc = 0.
    CASE wa_t052-zdart.
      WHEN 'C'.
        r_dt_vencimento = i_dt_doc.
      WHEN 'D'.
        r_dt_vencimento = i_dt_doc.
      WHEN 'B'.
        r_dt_vencimento = i_dt_doc.
      WHEN OTHERS.
        r_dt_vencimento = i_dt_doc.
    ENDCASE.

    IF NOT wa_t052-zfael IS INITIAL.
      CONCATENATE r_dt_vencimento(6) wa_t052-zfael INTO r_dt_vencimento.
    ENDIF.

    IF NOT wa_t052-zmona IS INITIAL.
      CALL FUNCTION 'MONTH_PLUS_DETERMINE'
        EXPORTING
          months  = wa_t052-zmona
          olddate = r_dt_vencimento
        IMPORTING
          newdate = r_dt_vencimento.
    ENDIF.

    tag1 = wa_t052-ztag1.
    tag2 = wa_t052-ztag2.
    tag3 = wa_t052-ztag3.

    CALL FUNCTION 'J_1B_FI_NETDUE'
      EXPORTING
        zfbdt   = r_dt_vencimento
        zbd1t   = tag1
        zbd2t   = tag2
        zbd3t   = tag3
        zstg1   = wa_t052-zstg1
        zsmn1   = wa_t052-zsmn1
        zstg2   = wa_t052-zstg2
        zsmn2   = wa_t052-zsmn2
        zstg3   = wa_t052-zstg3
        zsmn3   = wa_t052-zsmn3
      IMPORTING
        duedate = help_due_date
      EXCEPTIONS
        OTHERS  = 1.

    IF sy-subrc = 0.
      r_dt_vencimento  = help_due_date.
    ENDIF.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form f_monta_dados
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM f_monta_dados .

  DATA: lr_dt_fatura TYPE RANGE OF datum,
        lw_saida     TYPE ty_saida.

  APPEND INITIAL LINE TO lr_dt_fatura ASSIGNING FIELD-SYMBOL(<fs_dt_fatura>).
  <fs_dt_fatura>-sign = 'I'.
  <fs_dt_fatura>-option = 'BT'.
  <fs_dt_fatura>-high = sy-datum - 1.
  <fs_dt_fatura>-low = <fs_dt_fatura>-high - 30.

  SELECT *
    FROM zsdt0225
    INTO TABLE @DATA(lt_225)
    WHERE dt_fatura IN @lr_dt_fatura
      AND ( doc_lcto = @space
       OR   doc_lcto = '0' ).
  IF sy-subrc IS INITIAL.

    SELECT *
      FROM mara
      INTO TABLE @DATA(lt_mara)
      FOR ALL ENTRIES IN @lt_225
      WHERE matnr = @lt_225-cod_material.
    IF sy-subrc IS INITIAL.
      SORT lt_mara BY matnr.
    ENDIF.

    SELECT *
      FROM j_1bnfdoc
      INTO TABLE it_j_1bnfdoc
      FOR ALL ENTRIES IN lt_225
      WHERE docnum = lt_225-docnum.
    IF sy-subrc IS INITIAL.
      SORT it_j_1bnfdoc BY docnum.
    ENDIF.

    SELECT *
         FROM j_1bbranch INTO TABLE @DATA(lt_bbranch)
        FOR ALL ENTRIES IN @lt_225
        WHERE branch EQ @lt_225-cl_codigo+6(4).
    IF sy-subrc = 0.
      SORT lt_bbranch BY branch.

      SELECT  *
        FROM zsdt0229 INTO TABLE @DATA(lt_229)
        FOR ALL ENTRIES IN @lt_bbranch
      WHERE bukrs  EQ @lt_bbranch-bukrs.

      IF sy-subrc IS INITIAL.
        SORT lt_229 BY bukrs auart matkl.
      ENDIF.
    ENDIF.

  ENDIF.

  LOOP AT lt_225 ASSIGNING FIELD-SYMBOL(<fs_225>).

    READ TABLE lt_mara ASSIGNING FIELD-SYMBOL(<fs_mara>)
    WITH KEY matnr = <fs_225>-cod_material
    BINARY SEARCH.
    IF sy-subrc IS INITIAL.

      READ TABLE lt_bbranch ASSIGNING FIELD-SYMBOL(<fs_branch>)
      WITH KEY branch = <fs_225>-cl_codigo+6(4)
      BINARY SEARCH.
      IF sy-subrc IS INITIAL.

        READ TABLE lt_229 INTO DATA(wa_229)
        WITH KEY bukrs = <fs_branch>-bukrs
                 auart = <fs_225>-auart
                 matkl = <fs_mara>-matkl
        BINARY SEARCH.
        IF sy-subrc = 0.
          lw_saida-operacao      =  wa_229-operacao.
        ENDIF.
      ENDIF.

    ENDIF.

    lw_saida-werks_serv = <fs_225>-werks_serv.
    lw_saida-filial     = <fs_225>-cl_codigo+6(4).
    lw_saida-matnr      = <fs_225>-matnr_ov.
    lw_saida-docnum     = <fs_225>-docnum.
    lw_saida-id_seq     = <fs_225>-id_seq.
    lw_saida-dt_fatura  = sy-datum.
    lw_saida-vlr_brl    = <fs_225>-vlr_brl.

    READ TABLE it_j_1bnfdoc ASSIGNING FIELD-SYMBOL(<fs_j_1bnfdoc>) WITH KEY docnum = <fs_225>-docnum
    BINARY SEARCH.
    IF sy-subrc = 0.
      lw_saida-nf_saida = <fs_j_1bnfdoc>-nfnum.
      lw_saida-series   = <fs_j_1bnfdoc>-series.
    ENDIF.


    PERFORM f_nota_remessa USING lw_saida
                        CHANGING lv_seq_lcto.
    IF lv_seq_lcto IS NOT INITIAL.
      CALL FUNCTION 'ZNFW_PROCESSA_SEQ_LCTO'
        EXPORTING
          i_seq_lcto = lv_seq_lcto.
    ENDIF.

  ENDLOOP.

ENDFORM.
