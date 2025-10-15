*&---------------------------------------------------------------------*
*& Report  Z_GRAVA_ZIB_IMP
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  z_grava_zib_imp.

TYPE-POOLS: pmst.
TYPES: BEGIN OF ty_zib_contabil.
         INCLUDE STRUCTURE zib_contabil.
TYPES:   mark TYPE c,
       END OF ty_zib_contabil.

"Variáveis.
DATA: gv_object_reference TYPE c LENGTH 40,
      gv_job_name         TYPE tbtcm-jobname,
      gv_job_count        TYPE tbtcm-jobcount.

DATA: it_zimp_lanc_impost TYPE TABLE OF zimp_lanc_impost WITH HEADER LINE,
      it_zimp_lanc_imp_ct TYPE TABLE OF zimp_lanc_imp_ct WITH HEADER LINE,
      it_zimp_cad_imposto TYPE TABLE OF zimp_cad_imposto WITH HEADER LINE,
      it_zib_contabil     TYPE TABLE OF ty_zib_contabil,
      wa_zib_contabil     TYPE ty_zib_contabil,
      tg_msg              TYPE TABLE OF bdcmsgcoll WITH HEADER LINE,
      wg_par              TYPE ctu_params.

SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
  PARAMETERS: p_lote  TYPE zimp_lanc_impost-lote.
SELECTION-SCREEN: END OF BLOCK b1.

START-OF-SELECTION.
  PERFORM seleciona_dados.
  PERFORM organiza_dados.
*&---------------------------------------------------------------------*
*&      Form  SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM seleciona_dados .
  IF sy-batch IS NOT INITIAL.
    CALL FUNCTION 'GET_JOB_RUNTIME_INFO'
      IMPORTING
        jobname         = gv_job_name
        jobcount        = gv_job_count
      EXCEPTIONS
        no_runtime_info = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
    ENDIF.

    SPLIT gv_job_name AT '|' INTO DATA(lv_desc)
                                    DATA(lv_lote).
    IF lv_lote IS NOT INITIAL.
      p_lote = lv_lote.
    ENDIF.

  ENDIF.
  SELECT *
    INTO TABLE it_zimp_lanc_impost
    FROM zimp_lanc_impost
    WHERE lote = p_lote
    AND loekz  = ''.

  CHECK it_zimp_lanc_impost[] IS NOT INITIAL.

  SELECT *
    INTO TABLE it_zimp_lanc_imp_ct
    FROM zimp_lanc_imp_ct
    FOR ALL ENTRIES IN it_zimp_lanc_impost
    WHERE doc_imposto = it_zimp_lanc_impost-doc_imposto
    AND   bukrs       = it_zimp_lanc_impost-bukrs.


  SELECT *
    INTO TABLE it_zimp_cad_imposto
    FROM zimp_cad_imposto
    FOR ALL ENTRIES IN it_zimp_lanc_imp_ct
    WHERE cod_imposto = it_zimp_lanc_imp_ct-cod_imposto.

ENDFORM.                    " SELECIONA_DADOS
*&---------------------------------------------------------------------*
*&      Form  ORGANIZA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM organiza_dados .
  DATA vseq TYPE i VALUE 0.
  SORT: it_zimp_lanc_impost BY doc_imposto bukrs,
        it_zimp_lanc_imp_ct BY doc_imposto bukrs,
        it_zimp_cad_imposto BY cod_imposto.

  LOOP AT it_zimp_lanc_impost .
    vseq = 0.
    LOOP AT it_zimp_lanc_imp_ct WHERE doc_imposto = it_zimp_lanc_impost-doc_imposto
                                AND   bukrs       = it_zimp_lanc_impost-bukrs.
      IF it_zimp_lanc_imp_ct-valor_imp NE 0.
        IF it_zimp_lanc_imp_ct-seqitem NE 0.
          vseq = it_zimp_lanc_imp_ct-seqitem.
        ELSE.
          ADD 1 TO vseq.
        ENDIF.
        CONCATENATE 'ZP' it_zimp_lanc_impost-bukrs it_zimp_lanc_impost-doc_imposto sy-datum+0(4) INTO wa_zib_contabil-obj_key.
        wa_zib_contabil-seqitem   = vseq.
        CONCATENATE 'LOTE-' it_zimp_lanc_impost-lote INTO wa_zib_contabil-xblnr.
        wa_zib_contabil-bschl     = it_zimp_lanc_imp_ct-bschl.
        wa_zib_contabil-umskz     = it_zimp_lanc_imp_ct-umskz.
        wa_zib_contabil-gsber     = it_zimp_lanc_impost-gsber.
        wa_zib_contabil-bukrs     = it_zimp_lanc_impost-bukrs.
        wa_zib_contabil-interface   = '35'.
        wa_zib_contabil-bktxt     = ''.
        CONCATENATE  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO wa_zib_contabil-bldat SEPARATED BY '.'.
        CONCATENATE  sy-datum+6(2) sy-datum+4(2) sy-datum+0(4) INTO wa_zib_contabil-budat SEPARATED BY '.'.

        wa_zib_contabil-gjahr     = sy-datum+0(4).
        wa_zib_contabil-monat     = sy-datum+4(2).
        wa_zib_contabil-blart     = 'TB'.
        IF it_zimp_lanc_imp_ct-lifnr IS INITIAL AND it_zimp_lanc_imp_ct-kunnr IS INITIAL.
          wa_zib_contabil-hkont     = it_zimp_lanc_imp_ct-hkont.
        ELSEIF it_zimp_lanc_imp_ct-kunnr IS NOT INITIAL.
          wa_zib_contabil-hkont     = it_zimp_lanc_imp_ct-kunnr.
        ELSE.
          wa_zib_contabil-hkont     = it_zimp_lanc_imp_ct-lifnr.
        ENDIF.
        wa_zib_contabil-kostl     = it_zimp_lanc_imp_ct-kostl.
        wa_zib_contabil-aufnr     = it_zimp_lanc_imp_ct-aufnr.
        wa_zib_contabil-prctr     = it_zimp_lanc_imp_ct-prctr.
        wa_zib_contabil-matnr     = it_zimp_lanc_imp_ct-matnr.

        " Se moeda do documento não for BRL pegar como valor a moeda forte
        IF it_zimp_lanc_impost-waers = 'BRL'.
          IF it_zimp_lanc_imp_ct-valor_imp LT 0.
            wa_zib_contabil-wrbtr     = it_zimp_lanc_imp_ct-valor_imp * -1.
          ELSE.
            wa_zib_contabil-wrbtr     = it_zimp_lanc_imp_ct-valor_imp.
          ENDIF.
        ELSE.
          IF it_zimp_lanc_imp_ct-valor_for LT 0.
            wa_zib_contabil-wrbtr     = it_zimp_lanc_imp_ct-valor_for * -1.
          ELSE.
            wa_zib_contabil-wrbtr     = it_zimp_lanc_imp_ct-valor_for .
          ENDIF.
        ENDIF.
        wa_zib_contabil-waers     = it_zimp_lanc_impost-waers.


        wa_zib_contabil-bupla     = it_zimp_lanc_imp_ct-gsber.

        READ TABLE it_zimp_cad_imposto WITH KEY cod_imposto = it_zimp_lanc_imp_ct-cod_imposto BINARY SEARCH.
        " CONCATENATE  IT_ZIMP_LANC_IMP_CT-COD_IMPOSTO IT_ZIMP_CAD_IMPOSTO-DESCR_IMPOSTO INTO WA_ZIB_CONTABIL-SGTXT SEPARATED BY '-'.
        wa_zib_contabil-sgtxt = it_zimp_lanc_impost-observacao.

        " Moeda Interna Sempre 'BRL'
        wa_zib_contabil-waers_i   = 'BRL'. " IT_ZIMP_LANC_IMPOST-WAERS.
        IF it_zimp_lanc_imp_ct-valor_imp LT 0.
          wa_zib_contabil-dmbtr     = it_zimp_lanc_imp_ct-valor_imp * -1.
        ELSE.
          wa_zib_contabil-dmbtr     = it_zimp_lanc_imp_ct-valor_imp .
        ENDIF.

        IF NOT it_zimp_lanc_impost-waers_f IS INITIAL.
          wa_zib_contabil-waers_f   = it_zimp_lanc_impost-waers_f.
          IF it_zimp_lanc_imp_ct-valor_for LT 0.
            wa_zib_contabil-dmbe2     = it_zimp_lanc_imp_ct-valor_for * -1.
          ELSE.
            wa_zib_contabil-dmbe2     = it_zimp_lanc_imp_ct-valor_for .
          ENDIF.
        ELSE.
          wa_zib_contabil-waers_f   = ''.
          wa_zib_contabil-dmbe2     = 0.
        ENDIF.

        wa_zib_contabil-zlsch         = 'P'.

        wa_zib_contabil-rg_atualizado	=	'N'.
        CONCATENATE  it_zimp_lanc_impost-dt_venc+6(2) it_zimp_lanc_impost-dt_venc+4(2) it_zimp_lanc_impost-dt_venc+0(4) INTO wa_zib_contabil-zfbdt SEPARATED BY '.'.
        wa_zib_contabil-hbkid     = it_zimp_lanc_impost-hbkid.


        INSERT INTO  zib_contabil VALUES wa_zib_contabil.
        IF sy-subrc NE 0.
          ROLLBACK WORK.
        ELSE.
          COMMIT WORK.
        ENDIF.
        CLEAR  wa_zib_contabil.
      ENDIF.
    ENDLOOP.
  ENDLOOP.
ENDFORM.                    " ORGANIZA_DADOS
