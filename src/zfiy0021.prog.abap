*&---------------------------------------------------------------------*
*& Report  ZFIY0021
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zfiy0021.



TYPES : BEGIN OF ty_bkpf_aux ,
          bukrs	  TYPE bkpf-bukrs,
          belnr	  TYPE bkpf-belnr,
          gjahr	  TYPE bkpf-gjahr,
          awkey   TYPE bkpf-awkey,
          stblg   TYPE bkpf-stblg,
          bktxt   TYPE bkpf-belnr,
          xobjkey TYPE c LENGTH 1,
        END OF ty_bkpf_aux.

DATA: wa_zfiyt0029        TYPE zfiyt0029,
      wa_zfiyt0028        TYPE zfiyt0028,
      wa_retencao         TYPE zretencao,
      wa_retencao_item    TYPE zretencao_item,
      wa_with_item        TYPE with_item,
      wa_bkpf             TYPE bkpf,
      wa_bkpf_2           TYPE bkpf,
      wa_reguh            TYPE reguh,
      wa_regup            TYPE regup,
      wa_bsak             TYPE bsak,
      "WA_DOCAUX        TYPE BSAK,
      wa_bkpf_aux         TYPE ty_bkpf_aux,
      wa_zmmt_ee_zgr_docs TYPE zmmt_ee_zgr_docs,
      wa_zmmt_ee_zgr      TYPE zmmt_ee_zgr.

DATA: t_zfiyt0029         TYPE TABLE OF zfiyt0029,
      t_zfiyt0028         TYPE TABLE OF zfiyt0028,
      t_retencao          TYPE TABLE OF zretencao,
      t_retencao_item     TYPE TABLE OF zretencao_item,
      it_reguh            TYPE TABLE OF reguh,
      it_with_item        TYPE TABLE OF with_item,
      it_regup            TYPE TABLE OF regup,
      it_bsak             TYPE TABLE OF bsak,
      it_bkpf             TYPE TABLE OF bkpf,
      it_bkpf_2           TYPE TABLE OF bkpf,
      "IT_DOCAUX       TYPE TABLE OF BSAK,
      it_bkpf_aux         TYPE TABLE OF ty_bkpf_aux,
      it_zmmt_ee_zgr_docs TYPE TABLE OF zmmt_ee_zgr_docs,
      it_zmmt_ee_zgr      TYPE TABLE OF zmmt_ee_zgr.

START-OF-SELECTION.
  PERFORM : z_seleciona_dados,
            z_processa.

*&---------------------------------------------------------------------*
*&      Form  Z_seleciona_dados
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_seleciona_dados .

  DATA: vl_xvorl   LIKE reguh-xvorl,
        vl_dorigin LIKE reguh-dorigin,
        vl_koart   LIKE regup-koart,
        vl_len     TYPE i,
        vl_dtini   TYPE regup-laufd,
        vg_job     TYPE i.


  SELECT SINGLE COUNT(*) INTO vg_job
    FROM tbtco
   WHERE jobname EQ 'RET_PAGTO_RETENCAO'
     AND status EQ 'R'.

  IF ( vg_job EQ 1 ).


    vl_dtini = sy-datum - 1.

    SELECT *"VBLNR BUKRS BELNR GJAHR BUZEI KOART LIFNR WAERS
      FROM regup
      INTO TABLE it_regup
     WHERE laufd  >= vl_dtini
       AND laufd  <= sy-datum
       AND xvorl  = ''
       AND zbukr  = '0100'
       AND vblnr  NE ''
       AND blart  IN ('ZG','ZY','ZI','ZH', 'RE', 'ZF', 'AB', 'LM').

    CHECK ( sy-subrc EQ 0 ).

    SELECT *
      FROM bsak
      INTO TABLE it_bsak
       FOR ALL ENTRIES IN it_regup
     WHERE bukrs = it_regup-zbukr
       AND belnr = it_regup-vblnr.

    SELECT *
      FROM with_item
      INTO TABLE it_with_item
       FOR ALL ENTRIES IN it_regup
     WHERE bukrs  = it_regup-bukrs
       AND belnr  = it_regup-vblnr
       AND gjahr  = it_regup-gjahr.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf_2
       FOR ALL ENTRIES IN it_regup
     WHERE bukrs  = it_regup-bukrs
       AND belnr  = it_regup-belnr
       AND gjahr  = it_regup-gjahr.

    SELECT *
      FROM bkpf
      INTO TABLE it_bkpf
       FOR ALL ENTRIES IN it_regup
     WHERE bukrs  = it_regup-bukrs
       AND belnr  = it_regup-vblnr
       AND gjahr  = it_regup-gjahr.

    LOOP AT it_bkpf_2 INTO wa_bkpf_2.
      CLEAR wa_bkpf_aux.
      wa_bkpf_aux-bukrs  = wa_bkpf_2-bukrs.
      wa_bkpf_aux-belnr  = wa_bkpf_2-belnr.
      wa_bkpf_aux-gjahr  = wa_bkpf_2-gjahr.
      wa_bkpf_aux-stblg  = wa_bkpf_2-stblg.

      vl_len =  strlen( wa_bkpf_2-awkey ).

      IF wa_bkpf_2-awkey(1) = 'S'.

        vl_len              = strlen( wa_bkpf_2-awkey ) - 6.
        wa_bkpf_aux-awkey   = wa_bkpf_2-awkey+2(vl_len).
        wa_bkpf_aux-bktxt   = wa_bkpf_2-bktxt.
        wa_bkpf_aux-xobjkey = 'S'.

      ELSEIF vl_len > 10.

        vl_len              = strlen( wa_bkpf_2-awkey ) - 4.
        wa_bkpf_aux-bktxt   = wa_bkpf_2-awkey(vl_len).
        wa_bkpf_aux-xobjkey = ''.

      ELSE.

        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_bkpf_2-awkey
          IMPORTING
            output = wa_bkpf_aux-bktxt.

        wa_bkpf_aux-awkey  = wa_bkpf_2-awkey.
        wa_bkpf_aux-xobjkey = ''.

      ENDIF.

      APPEND wa_bkpf_aux TO it_bkpf_aux.

    ENDLOOP.

    "MIRO
    SELECT *
      FROM zmmt_ee_zgr_docs
      INTO TABLE it_zmmt_ee_zgr_docs
       FOR ALL ENTRIES IN it_bkpf_aux
     WHERE bukrs    = it_bkpf_aux-bukrs
       AND ft_belnr	= it_bkpf_aux-bktxt
       AND ft_gjahr	= it_bkpf_aux-gjahr.

    SELECT *
      FROM zmmt_ee_zgr
      INTO TABLE it_zmmt_ee_zgr
       FOR ALL ENTRIES IN it_zmmt_ee_zgr_docs
     WHERE obj_key = it_zmmt_ee_zgr_docs-obj_key.
  ENDIF.


ENDFORM.                    " Z_RETENCAO_ARG

*&---------------------------------------------------------------------*
*&      Form  Z_PROCESSA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM z_processa .

  DATA : vl_count   TYPE i,
         vl_len     TYPE i,
         vl_obj_key TYPE zfiyt0029-obj_key.

  SORT : it_with_item         BY bukrs belnr gjahr ,
         it_bkpf              BY bukrs belnr gjahr ,
         it_bkpf_aux          BY bukrs belnr gjahr ,
         it_bkpf_2            BY bukrs belnr gjahr ,
         it_zmmt_ee_zgr_docs  BY bukrs ft_belnr ft_gjahr,
         it_zmmt_ee_zgr       BY obj_key,
         it_bsak              BY bukrs belnr.

  vl_count = 0.

  LOOP AT it_regup INTO wa_regup.

    READ TABLE it_bkpf INTO wa_bkpf WITH KEY bukrs  = wa_regup-bukrs
                                             belnr  = wa_regup-vblnr
                                             gjahr  = wa_regup-gjahr BINARY SEARCH.

    IF wa_bkpf-stblg NE '' .
      CONTINUE.
    ENDIF.

    READ TABLE it_bsak INTO wa_bsak WITH KEY bukrs  = wa_regup-zbukr
                                             belnr  = wa_regup-vblnr BINARY SEARCH.

    READ TABLE it_bkpf_aux INTO wa_bkpf_aux WITH KEY bukrs  = wa_regup-bukrs
                                                     belnr  = wa_regup-belnr
                                                     gjahr  = wa_regup-gjahr BINARY SEARCH.

    READ TABLE it_zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs WITH  KEY bukrs     = wa_bkpf_aux-bukrs
                                                                      ft_belnr  = wa_bkpf_aux-bktxt
                                                                      ft_gjahr  = wa_bkpf_aux-gjahr BINARY SEARCH.

    READ TABLE it_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY obj_key = wa_zmmt_ee_zgr_docs-obj_key BINARY SEARCH.

    IF wa_bkpf_aux-xobjkey = 'S'. "Split
      vl_obj_key = wa_bkpf_aux-awkey.
    ELSE."Miro
      vl_obj_key = wa_zmmt_ee_zgr_docs-obj_key .
    ENDIF.

    SELECT SINGLE *
      INTO wa_zfiyt0029
      FROM zfiyt0029
     WHERE obj_key = vl_obj_key
       AND augbl   = wa_regup-vblnr.

    IF sy-subrc IS NOT INITIAL.

      CLEAR : wa_zfiyt0029 .

      READ TABLE it_with_item INTO wa_with_item WITH KEY bukrs  = wa_regup-bukrs
                                                         belnr  = wa_regup-vblnr
                                                         gjahr  = wa_regup-gjahr BINARY SEARCH .

      wa_zfiyt0029-obj_key     = vl_obj_key.
      wa_zfiyt0029-bukrs       = wa_regup-bukrs.
      wa_zfiyt0029-lifnr       = wa_regup-lifnr.
      wa_zfiyt0029-augdt       = wa_regup-zfbdt + wa_regup-zbd1t."WA_REGUP-BUDAT.
      wa_zfiyt0029-augbl       = wa_regup-vblnr.
      wa_zfiyt0029-waers       = wa_regup-waers.
      wa_zfiyt0029-dmbtr       = wa_regup-dmbtr.
      wa_zfiyt0029-id_corredor = wa_zmmt_ee_zgr-lifnr.
      wa_zfiyt0029-dt_comp     = wa_bsak-augdt."Data de compensação
      wa_zfiyt0029-zdt_atlz    = sy-datum.
      wa_zfiyt0029-zhr_atlz    = sy-uzeit.
      wa_zfiyt0029-zrg_atlz    = '0'.

      MOVE-CORRESPONDING wa_zfiyt0029 TO wa_retencao.
      APPEND wa_retencao TO t_retencao.

      INSERT INTO zfiyt0029 VALUES wa_zfiyt0029.

      LOOP AT it_with_item  INTO wa_with_item WHERE bukrs  = wa_regup-bukrs AND
                                                    belnr  = wa_regup-vblnr AND
                                                    gjahr  = wa_regup-gjahr .

        CONCATENATE wa_bkpf-brnch wa_with_item-ctnumber+2(8) INTO wa_zfiyt0028-zcertificado.
        wa_zfiyt0028-obj_key       = wa_zfiyt0029-obj_key.
        wa_zfiyt0028-wi_tax_code   = wa_with_item-witht.
        wa_zfiyt0028-wt_withcd     = wa_with_item-wt_withcd.
        wa_zfiyt0028-wi_tax_base   = wa_with_item-wt_qsshh.
        wa_zfiyt0028-wi_tax_amt    = wa_with_item-wt_qbshh.
        wa_zfiyt0028-zdt_atlz      = sy-datum.
        wa_zfiyt0028-zhr_atlz      = sy-uzeit.
        wa_zfiyt0028-nr_ordem_pago = wa_with_item-belnr.
        wa_zfiyt0028-zrg_atlz      = '0'.

        MOVE-CORRESPONDING wa_zfiyt0028 TO wa_retencao_item.
        APPEND wa_retencao_item TO t_retencao_item.

        INSERT INTO zfiyt0028 VALUES wa_zfiyt0028.

        CLEAR wa_zfiyt0028.

      ENDLOOP.

      DELETE it_with_item WHERE  bukrs  = wa_regup-bukrs AND
                                 belnr  = wa_regup-vblnr AND
                                 gjahr  = wa_regup-gjahr .

      vl_count = vl_count + 1.
    ENDIF.

    CLEAR: wa_zfiyt0029, wa_with_item, wa_zfiyt0028, wa_regup, wa_bkpf_aux, wa_zmmt_ee_zgr_docs, vl_obj_key.

  ENDLOOP.
* ---> S4 Migration - 09/08/2023 - GB - Inicio
*  DATA: lo_zco_z_fi_out_pagamento_po TYPE REF TO zco_z_fi_outbound_pagamento_po,
*        ls_input TYPE zzfi_outbound_pagamento_inpu1.
* <--- S4 Migration - 09/08/2023 - GB - Fim
  IF vl_count > 0 .

*--> 25.08.2023 15:38:06 - Migração S4 – ML - Início
*    CALL FUNCTION 'Z_FI_OUTBOUND_PAGAMENTO_ITEM' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETORNO_PAGAMENTO_ITEM'
*      TABLES
*        T_RETORNO_RETENCAO_ITEM = T_RETENCAO_ITEM[].
* ---> S4 Migration - 09/08/2023 - GB - Inicio
*    CALL FUNCTION 'Z_FI_OUTBOUND_PAGAMENTO' IN BACKGROUND TASK
*      DESTINATION 'XI_SIGAM_RETORNO_PAGAMENTO'
*      TABLES
*        T_RETORNO_RETENCAO = T_RETENCAO[].

*    CREATE OBJECT lo_zco_z_fi_out_pagamento_po.
*
*    LOOP AT T_RETENCAO[] INTO DATA(ls_retencao).
*
*      APPEND INITIAL LINE TO ls_input-t_retorno_retencao-item ASSIGNING FIELD-SYMBOL(<fs_input>).
*
*      MOVE-CORRESPONDING ls_retencao TO <fs_input>.
*
*    ENDLOOP.
*    TRY.
*      lo_zco_z_fi_out_pagamento_po->z_fi_outbound_pagamento(
*        EXPORTING
*          input  = ls_input
*      ).
*      CATCH cx_ai_system_fault. " Communication Error
*    ENDTRY.
** <--- S4 Migration - 09/08/2023 - GB - Fim

    DATA: lv_rfc TYPE rfcdest,
          lv_fm  TYPE rs38l_fnam.

   " lv_rfc = 'Z_FI_OUTBOUND_PAGAMENTO'.

 CONSTANTS: c_fm TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_PAGAMENTO'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          t_retorno_retencao = t_retencao[].
    ELSE.
      CALL FUNCTION c_fm IN BACKGROUND TASK
        TABLES
          t_retorno_retencao = t_retencao[].
    ENDIF.


CONSTANTS: c_fm_i TYPE rs38l_fnam VALUE 'Z_FI_OUTBOUND_PAGAMENTO'.


    lv_rfc = 'Z_FI_OUTBOUND_PAGAMENTO_ITEM'.

    CALL FUNCTION 'ZFMCPI_UTIL_GET_RFC'
      EXPORTING
        i_fm          = c_fm_i
      IMPORTING
        e_rfc         = lv_rfc
      EXCEPTIONS
        no_rfc        = 1
        no_rfc_config = 2
        OTHERS        = 3.

    IF sy-subrc EQ 0.
      CALL FUNCTION c_fm_i IN BACKGROUND TASK
        DESTINATION lv_rfc
        TABLES
          t_retorno_retencao_item = t_retencao_item[].
    ELSE.
      CALL FUNCTION c_fm_i IN BACKGROUND TASK
        TABLES
          t_retorno_retencao_item = t_retencao_item[].
    ENDIF.

    COMMIT WORK.
*<-- 25.08.2023 15:38:06 - Migração S4 – ML – Fim

  ENDIF.

ENDFORM.                    " Z_RETENCAO_ARG
