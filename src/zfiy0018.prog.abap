*----------------------------------------------------------------------*
* Report  ZFIY0018                                                     *
* Descrição  : SPLIT – Facturas (Mercaderias)                          *
*                                                                      *
*----------------------------------------------------------------------*
* Autor      : Paulo Bonetti                          Data: 25/10/2012 *
* Observações: Desenvolvimento inicial do Programa                     *
*----------------------------------------------------------------------*

REPORT  zfiy0018.

*----------------------------------------------------------------------*
* TYPE POOLS
*----------------------------------------------------------------------*
TYPE-POOLS: icon.

TABLES: zmmt_ee_zgr, rbkp, zib_contabil.

*----------------------------------------------------------------------*
* ESTRUTURAS
*----------------------------------------------------------------------*
TYPES:
  BEGIN OF ty_zmmt_ee_zgr,
    obj_key        TYPE zmmt_ee_zgr-obj_key,
    po_number      TYPE zmmt_ee_zgr-po_number,
    ref_doc_no     TYPE zmmt_ee_zgr-ref_doc_no,
    vr_bruto       TYPE zmmt_ee_zgr-vr_bruto,
    plant          TYPE zmmt_ee_zgr-plant,
    doc_date       TYPE zmmt_ee_zgr-doc_date,
    pstng_date     TYPE zmmt_ee_zgr-pstng_date,
    lifnr          TYPE zmmt_ee_zgr-lifnr,
    blart          TYPE zmmt_ee_zgr-blart,
    nt_remessa     TYPE zmmt_ee_zgr-nt_remessa,
    comp_code      TYPE zmmt_ee_zgr-comp_code,
    desm_pagos     TYPE zmmt_ee_zgr-desm_pagos,
    tp_operacao    TYPE zmmt_ee_zgr-tp_operacao,
    vl_cmv         TYPE zmmt_ee_zgr-vl_cmv,
    text1          TYPE zmmt_ee_zgr-text1,
    interface_miro TYPE zmmt_ee_zgr-interface_miro,
    zrg_atlz       TYPE zmmt_ee_zgr-zrg_atlz,
    belnr          TYPE zmmt_ee_zgr-belnr,
  END OF ty_zmmt_ee_zgr,

  BEGIN OF ty_zmmt_ee_zgr_aux,
    comp_code TYPE zmmt_ee_zgr-comp_code,
    awkey     TYPE   bkpf-awkey,
    obj_key   TYPE zmmt_ee_zgr-obj_key,
  END OF ty_zmmt_ee_zgr_aux,

  BEGIN OF ty_zmmt_ee_zgr_docs,
    obj_key  TYPE zmmt_ee_zgr_docs-obj_key,
    ft_belnr TYPE zmmt_ee_zgr_docs-ft_belnr,
    ft_gjahr TYPE zmmt_ee_zgr_docs-ft_gjahr,
  END OF ty_zmmt_ee_zgr_docs,

  BEGIN OF ty_zmmt_ee_zgr_docs_aux,
    awkey	    TYPE bkpf-awkey,
    ft_belnr  TYPE zmmt_ee_zgr_docs-ft_belnr,
    ft_gjahr  TYPE zmmt_ee_zgr_docs-ft_gjahr,
    po_number TYPE zmmt_ee_zgr-po_number,
  END OF ty_zmmt_ee_zgr_docs_aux,

  BEGIN OF ty_ekbe,
    ebeln TYPE ekbe-ebeln,
    belnr TYPE ekbe-belnr,
    gjahr TYPE ekbe-gjahr,
    vgabe TYPE ekbe-vgabe,
    shkzg TYPE ekbe-shkzg,
  END OF ty_ekbe,

  BEGIN OF ty_lfbw,
    lifnr TYPE lfbw-lifnr,
    witht TYPE lfbw-witht,
  END OF ty_lfbw,

  BEGIN OF ty_t042z,
    land1 TYPE t042z-land1,
    zlsch TYPE t042z-zlsch,
    text1 TYPE t042z-text1,
  END  OF ty_t042z,

  BEGIN OF ty_t012,
    hbkid TYPE t012-hbkid,
    banks TYPE t012-banks,
    bankl TYPE t012-bankl,
  END  OF ty_t012,

  BEGIN OF ty_rbkp,
    belnr	TYPE rbkp-belnr,
    gjahr	TYPE rbkp-gjahr,
    zbd1t TYPE rbkp-zbd1t,
    zfbdt TYPE rbkp-zfbdt,
    lifnr TYPE rbkp-lifnr,
    stblg TYPE rbkp-stblg,
  END OF ty_rbkp,

  BEGIN OF ty_zmmt_eeimp_zgr,
    obj_key     TYPE zmmt_eeimp_zgr-obj_key,
    wi_tax_code TYPE zmmt_eeimp_zgr-wi_tax_code,
    wt_withcd   TYPE zmmt_eeimp_zgr-wt_withcd,
    wi_tax_base TYPE zmmt_eeimp_zgr-wi_tax_base,
  END OF ty_zmmt_eeimp_zgr,

  BEGIN OF ty_ekko,
    ebeln TYPE ekko-ebeln,
    lifnr TYPE ekko-lifnr,
    bsart TYPE ekko-bsart,
  END  OF ty_ekko,

  BEGIN OF ty_lfbk,
    "lifnr TYPE lfbk-lifnr,
    bvtyp TYPE lfbk-bvtyp,
  END  OF ty_lfbk,

  BEGIN OF ty_lfa1,
    lifnr TYPE lfa1-lifnr,
    name1 TYPE lfa1-name1,
  END  OF ty_lfa1,

  BEGIN OF ty_bkpf,
    bukrs TYPE bkpf-bukrs,
    gjahr TYPE bkpf-gjahr,
    awkey TYPE bkpf-awkey,
    belnr TYPE bkpf-belnr,
    waers TYPE bkpf-waers,
    stblg TYPE bkpf-stblg,
  END  OF ty_bkpf,

  BEGIN OF ty_bset,
    bukrs TYPE bset-bukrs,
    belnr TYPE bset-belnr,
    gjahr TYPE bset-gjahr,
    hwbas TYPE bset-hwbas,
    hwste TYPE bset-hwste,
  END  OF ty_bset,

  BEGIN OF ty_setleaf,
    setname TYPE setleaf-setname,
    lineid  TYPE setleaf-lineid,
    valfrom TYPE setleaf-valfrom,
  END OF ty_setleaf,

  BEGIN OF ty_setlinet,
    setname  TYPE setlinet-setname,
    lineid   TYPE setlinet-lineid,
    descript TYPE setlinet-descript,
  END OF ty_setlinet,

  BEGIN OF ty_with_item,
    bukrs     TYPE with_item-bukrs,
    belnr     TYPE with_item-belnr,
    gjahr     TYPE with_item-gjahr,
    witht     TYPE with_item-witht,
    wt_withcd TYPE with_item-wt_withcd,
    wt_qsshh  TYPE with_item-wt_qsshh,
  END OF ty_with_item,

  BEGIN OF ty_bsak,
    augbl TYPE bsak-augbl,
    bukrs TYPE bsak-bukrs,
    belnr TYPE bsak-belnr,
    gjahr TYPE bsak-gjahr,
  END OF ty_bsak,

  BEGIN OF ty_t059z,
    witht     TYPE t059z-witht,
    wt_withcd TYPE t059z-wt_withcd,
    qsatz     TYPE t059z-qsatz,
  END OF ty_t059z,

  BEGIN OF ty_saida,
    obj_key        TYPE zmmt_ee_zgr-obj_key,
    status         TYPE icon-id,
    bukrs          TYPE zmmt_ee_zgr-comp_code,
    belnr          TYPE zmmt_ee_zgr_docs-ft_belnr,
    gjahr          TYPE zmmt_ee_zgr_docs-ft_gjahr,
    doc_date       TYPE zmmt_ee_zgr-doc_date,
    pstng_date     TYPE zmmt_ee_zgr-pstng_date,
    vr_bruto       TYPE zib_contabil-wrbtr,
    waers          TYPE bkpf-waers,
    prod           TYPE ekko-lifnr,
    nome_prod      TYPE lfa1-name1,
    total_prod     TYPE zib_contabil-wrbtr,
    corredor       TYPE zmmt_ee_zgr-lifnr,
    nome_corredor  TYPE lfa1-name1,
    valor          TYPE zib_contabil-wrbtr,
    nr_doc         TYPE  zib_contabil_chv-belnr,
    message        TYPE zib_contabil_err-message,
    po_number      TYPE zmmt_ee_zgr-po_number,
    vencimento     TYPE rbkp-zfbdt,
    tp_miro        TYPE string,
    lifnr_miro     TYPE lfa1-lifnr,
    augbl          TYPE bsak-augbl,
    desm_pagos     TYPE zmmt_ee_zgr-desm_pagos,
    nt_remessa     TYPE zmmt_ee_zgr-nt_remessa,
    tp_operacao    TYPE zmmt_ee_zgr-tp_operacao,
    vl_cmv         TYPE zmmt_ee_zgr-vl_cmv,
    interface_miro TYPE zmmt_ee_zgr-interface_miro,
    contrato       TYPE string,
  END OF ty_saida.

*&---------------------------------------------------------------------*
*& TABELA INTERNA
*&---------------------------------------------------------------------*
DATA: t_zmmt_ee_zgr          TYPE TABLE OF ty_zmmt_ee_zgr,
      t_zmmt_ee_zgr_1        TYPE TABLE OF ty_zmmt_ee_zgr,
      t_zmmt_ee_zgr_docs     TYPE TABLE OF ty_zmmt_ee_zgr_docs,
      t_zmmt_ee_zgr_docs_aux TYPE TABLE OF ty_zmmt_ee_zgr_docs_aux,
      t_zmmt_ee_zgr_aux      TYPE TABLE OF ty_zmmt_ee_zgr_aux,
      t_ekbe                 TYPE TABLE OF ty_ekbe,
      t_rbkp                 TYPE TABLE OF ty_rbkp,
      t_ekko                 TYPE TABLE OF ty_ekko,
      t_lfa1_ek              TYPE TABLE OF ty_lfa1,
      t_lfa1                 TYPE TABLE OF ty_lfa1,
      "T_LFBW                 TYPE TABLE OF TY_LFBW,  BUG 61386
      t_lfbw                 TYPE SORTED TABLE OF ty_lfbw WITH UNIQUE KEY lifnr witht,
      t_bkpf                 TYPE TABLE OF ty_bkpf,
      "T_BSET                 TYPE TABLE OF TY_BSET, BUG 61386
      t_bset                 TYPE STANDARD TABLE OF  ty_bset WITH NON-UNIQUE SORTED KEY key_belnr COMPONENTS belnr WITH NON-UNIQUE SORTED KEY  key_bukrs  COMPONENTS bukrs,
      t_setleaf              TYPE TABLE OF ty_setleaf,
      t_setlinet             TYPE TABLE OF ty_setlinet,
      "T_WITH_ITEM            TYPE TABLE OF TY_WITH_ITEM, BUG 61386
      t_with_item            TYPE STANDARD TABLE OF ty_with_item WITH NON-UNIQUE SORTED KEY key_belnr COMPONENTS belnr , "  BUKRS GJAHR, "WITH NON-UNIQUE SORTED KEY key_BELNR COMPONENTS BELNR ,

      "  T_T059Z                TYPE TABLE OF TY_T059Z, BUG 61386
      t_t059z                TYPE SORTED TABLE OF ty_t059z WITH UNIQUE KEY witht wt_withcd,
      t_saida                TYPE TABLE OF ty_saida WITH KEY obj_key,
      t_saida_bloq           TYPE TABLE OF ty_saida WITH KEY obj_key,
      t_saida_selection      TYPE TABLE OF ty_saida,
      t_zib_contabil         TYPE TABLE OF zib_contabil,
      t_zib_contabil_aux     TYPE TABLE OF zib_contabil,
      t_zib_contabil_chv     TYPE TABLE OF zib_contabil_chv,
      t_zib_contabil_chv_aux TYPE TABLE OF zib_contabil_chv,
      t_zib_contabil_err     TYPE TABLE OF zib_contabil_err,
      "  T_BSAK                 TYPE TABLE OF TY_BSAK, BUG 61386
      t_bsak                 TYPE SORTED TABLE OF ty_bsak  WITH UNIQUE KEY belnr bukrs gjahr,
      t_accountwt            TYPE TABLE OF bapiacwt09,
      t_return               TYPE TABLE OF bapiret2,
      t_documentheader       TYPE TABLE OF bapiache09,

      ti_bdc                 TYPE TABLE OF bdcdata,
      ti_msg                 TYPE TABLE OF bdcmsgcoll.

*&---------------------------------------------------------------------*
*& WORK AREA
*&---------------------------------------------------------------------*
DATA: wa_zmmt_ee_zgr          TYPE ty_zmmt_ee_zgr,
      wa_zmmt_ee_zgr_docs     TYPE ty_zmmt_ee_zgr_docs,
      wa_zmmt_ee_zgr_docs_aux TYPE ty_zmmt_ee_zgr_docs_aux,
      wa_zmmt_ee_zgr_aux      TYPE ty_zmmt_ee_zgr_aux,
      wa_ekbe                 TYPE ty_ekbe,
      wa_rbkp                 TYPE ty_rbkp,
      wa_ekko                 TYPE ty_ekko,
      wa_lfbk                 TYPE STANDARD TABLE OF ty_lfbk,
      it_lfbk                 TYPE STANDARD TABLE OF ty_lfbk,
      wa_lfa1_ek              TYPE ty_lfa1,
      wa_lfa1                 TYPE ty_lfa1,
      wa_lfbw                 TYPE ty_lfbw,
      wa_bkpf                 TYPE ty_bkpf,
      wa_bset                 TYPE ty_bset,
      wa_with_item            TYPE ty_with_item,
      wa_t059z                TYPE ty_t059z,
      wa_setleaf              TYPE ty_setleaf,
      wa_setlinet             TYPE ty_setlinet,
      wa_saida                TYPE ty_saida,
      wa_saida_l              TYPE ty_saida,
      wa_saida_selection      TYPE ty_saida,
      wa_zib_contabil         TYPE zib_contabil,
      wa_zib_contabil_i54     TYPE zib_contabil,
      wa_zib_contabil_chv     TYPE zib_contabil_chv,
      wa_zib_contabil_chv_i54 TYPE zib_contabil_chv,
      wa_zib_contabil_err     TYPE zib_contabil_err,
      wa_bsak                 TYPE ty_bsak,
      wa_accountwt            TYPE bapiacwt09,
      wa_return               TYPE bapiret2,
      wa_documentheader       TYPE bapiache09,

      wa_cont                 TYPE REF TO cl_gui_custom_container,
      wa_alv                  TYPE REF TO cl_gui_alv_grid,
      wa_layout               TYPE lvc_s_layo,

      wa_bdc                  TYPE bdcdata,
      wa_msg                  TYPE bdcmsgcoll.

*&---------------------------------------------------------------------*
*& Estrutura ALV
*&---------------------------------------------------------------------*

DATA:
  it_fcat            TYPE TABLE OF lvc_s_fcat,
  gs_alv_refres_cond TYPE lvc_s_stbl,
  formapgto          TYPE zib_contabil-zlsch VALUE 'T',
  bcoempresa         TYPE zib_contabil-hbkid VALUE 'N0016',
  dtvencto           TYPE zib_contabil-zfbdt,
  dtbudat            TYPE sy-datum,
  proc_v             TYPE c LENGTH 1,
  gs_variant_c       TYPE disvariant.
*&---------------------------------------------------------------------*
* ALV selection
*&---------------------------------------------------------------------*
DATA: it_selected_rows TYPE lvc_t_row,
      wa_selected_rows TYPE lvc_s_row.
**********************************************************************
* RANGE
**********************************************************************
DATA:       lr_bvtyp                TYPE RANGE OF lfbk-bvtyp.
DATA: vi001(4) TYPE c,
      vt001(4) TYPE c,
      vvazi(4) TYPE c.
*&---------------------------------------------------------------------*
*& TELA DE SELEÇÃO
*&---------------------------------------------------------------------*
SELECTION-SCREEN: BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.

  SELECT-OPTIONS: p_socied FOR zmmt_ee_zgr-comp_code OBLIGATORY,
                  p_proved FOR zmmt_ee_zgr-lifnr,
                  p_corred FOR zmmt_ee_zgr-lifnr,
                  p_nrcont FOR zmmt_ee_zgr-text1,
                  p_fecha  FOR zmmt_ee_zgr-pstng_date OBLIGATORY,
                  p_venc   FOR rbkp-zfbdt.

SELECTION-SCREEN: END OF BLOCK b1.

" Seleção de Campos (RadioButton)
SELECTION-SCREEN: BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-016.
  PARAMETERS: fc_proc  TYPE char1 RADIOBUTTON GROUP rb01 ,               " Processado
              fc_nproc TYPE char1  RADIOBUTTON GROUP rb01            , " Não Processado
              fc_ambos TYPE char1  RADIOBUTTON GROUP rb01 DEFAULT 'X'. " Ambos
SELECTION-SCREEN: END OF BLOCK b3.

" Seleção de Campos (RadioButton)
SELECTION-SCREEN: BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-024.
  PARAMETERS: p_comp  TYPE char1 RADIOBUTTON GROUP rb02 ,               " Processado
              p_n_doc TYPE char1  RADIOBUTTON GROUP rb02            , " Não Processado
              p_ambos TYPE char1  RADIOBUTTON GROUP rb02 DEFAULT 'X'. " Ambos
SELECTION-SCREEN: END OF BLOCK b4.

INITIALIZATION.
  gs_variant_c-report      = sy-repid.
*&---------------------------------------------------------------------*
*& START OF SELECTION
*&---------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM:  f_seleciona_dados USING ''. " Form seleciona dados

  DATA(_data_found) = ''.
  READ TABLE t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY interface_miro = '54'.
  IF sy-subrc EQ 0.
    _data_found = 'X'.
  ENDIF.
  IF ( t_zmmt_ee_zgr_docs_aux[] IS NOT INITIAL ) OR ( _data_found IS NOT INITIAL ). "ALRS 02.07.2015 docs nao encontrados
    PERFORM:  f_saida, " Form de saida
              f_alv. " Form ALV
    CALL SCREEN 0100.
  ENDIF.



END-OF-SELECTION.

*&---------------------------------------------------------------------*
*&      Form  F_SELECIONA_DADOS
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM f_seleciona_dados USING p_ren.

  TYPES: BEGIN OF ty_with_cd,
           wt_withcd TYPE with_item-wt_withcd.
  TYPES: END OF ty_with_cd.

  DATA: it_with_cd TYPE TABLE OF ty_with_cd WITH HEADER LINE.
  DATA r_belnr TYPE RANGE OF belnr_d.

  DATA: v1 TYPE c LENGTH 1,
        v2 TYPE c LENGTH 1.



  IF p_ren EQ 'X'.

    t_zmmt_ee_zgr_1 = t_zmmt_ee_zgr.

    REFRESH t_zmmt_ee_zgr.
    SELECT obj_key po_number ref_doc_no vr_bruto plant doc_date pstng_date lifnr blart nt_remessa comp_code desm_pagos tp_operacao
           vl_cmv text1 interface_miro zrg_atlz belnr
        FROM zmmt_ee_zgr
      INTO TABLE t_zmmt_ee_zgr
      FOR ALL ENTRIES IN t_zmmt_ee_zgr_1
     WHERE obj_key = t_zmmt_ee_zgr_1-obj_key.

  ELSE.

    REFRESH t_zmmt_ee_zgr.


    SELECT obj_key  po_number ref_doc_no vr_bruto plant doc_date pstng_date lifnr blart nt_remessa comp_code desm_pagos
           tp_operacao vl_cmv text1 interface_miro zrg_atlz belnr
      FROM zmmt_ee_zgr
      INTO TABLE t_zmmt_ee_zgr
     WHERE comp_code   IN p_socied
       AND tp_operacao IN ('01', '02', '03', '04','99')
       AND lifnr       NE ''
       AND cfop        NE '   /C'
       AND cfop        NE 'C'
       AND pstng_date	 IN p_fecha
       AND lifnr       IN p_corred
       AND zrg_atlz	   IN ('0', '1')
       AND text1       IN p_nrcont.

  ENDIF.

  IF t_zmmt_ee_zgr[] IS INITIAL.
    MESSAGE 'Entradas fiscais de compras ZGR no encontrados ' TYPE 'I'.
    EXIT.
  ENDIF.

  SELECT obj_key
         ft_belnr
         ft_gjahr
    FROM zmmt_ee_zgr_docs
    INTO TABLE t_zmmt_ee_zgr_docs
     FOR ALL ENTRIES IN t_zmmt_ee_zgr
   WHERE obj_key = t_zmmt_ee_zgr-obj_key .

  SORT t_zmmt_ee_zgr BY obj_key.

  LOOP AT t_zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs.
    READ TABLE t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY obj_key = wa_zmmt_ee_zgr_docs-obj_key BINARY SEARCH.

    CONCATENATE wa_zmmt_ee_zgr_docs-ft_belnr  wa_zmmt_ee_zgr_docs-ft_gjahr INTO wa_zmmt_ee_zgr_docs_aux-awkey .

    wa_zmmt_ee_zgr_docs_aux-ft_belnr  = wa_zmmt_ee_zgr_docs-ft_belnr.
    wa_zmmt_ee_zgr_docs_aux-ft_gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr.
    wa_zmmt_ee_zgr_docs_aux-po_number = wa_zmmt_ee_zgr-po_number.
    APPEND wa_zmmt_ee_zgr_docs_aux TO t_zmmt_ee_zgr_docs_aux.

  ENDLOOP.

  CLEAR: wa_zmmt_ee_zgr_docs.
  SORT t_zmmt_ee_zgr_docs_aux BY po_number ft_belnr ft_gjahr.
  IF t_zmmt_ee_zgr_docs_aux[] IS INITIAL. "ALRS 02.07.2015 docs nao encontrados
    READ TABLE t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY interface_miro = '54'.
    IF sy-subrc NE 0.
      MESSAGE 'Entradas fiscais de compras ZGR no encontrados ' TYPE 'I'.
      EXIT.
    ENDIF.
  ENDIF.

  IF t_zmmt_ee_zgr_docs_aux[] IS NOT INITIAL.
    SELECT ebeln
           belnr
           gjahr
           vgabe
           shkzg
      FROM ekbe
      INTO TABLE t_ekbe
       FOR ALL ENTRIES IN t_zmmt_ee_zgr_docs_aux
     WHERE ebeln  = t_zmmt_ee_zgr_docs_aux-po_number
       AND belnr  = t_zmmt_ee_zgr_docs_aux-ft_belnr
       AND gjahr  = t_zmmt_ee_zgr_docs_aux-ft_gjahr.
  ENDIF.

  IF t_zmmt_ee_zgr_docs[] IS NOT INITIAL.
    SELECT belnr
           gjahr
           zbd1t
           zfbdt
           lifnr
           stblg
      FROM rbkp
      INTO TABLE t_rbkp
       FOR ALL ENTRIES IN t_zmmt_ee_zgr_docs
     WHERE belnr  = t_zmmt_ee_zgr_docs-ft_belnr
       AND gjahr  = t_zmmt_ee_zgr_docs-ft_gjahr.
  ENDIF.

  SELECT ebeln
         lifnr
         bsart
    FROM ekko
    INTO TABLE t_ekko
     FOR ALL ENTRIES IN t_zmmt_ee_zgr
   WHERE ebeln  = t_zmmt_ee_zgr-po_number.

  IF t_ekko[] IS NOT INITIAL.
    SELECT lifnr
           name1
      FROM lfa1
      INTO TABLE t_lfa1_ek
       FOR ALL ENTRIES IN t_ekko
     WHERE lifnr = t_ekko-lifnr.

    IF t_lfa1_ek[] IS NOT INITIAL.
      SELECT lifnr witht
        FROM lfbw
        INTO TABLE t_lfbw
        FOR ALL ENTRIES IN t_lfa1_ek
        WHERE bukrs = p_socied-low
        AND   lifnr = t_lfa1_ek-lifnr
        AND   wt_subjct = 'X'. " sujeito
    ENDIF.
  ENDIF.

  SELECT lifnr
         name1
    FROM lfa1
    INTO TABLE t_lfa1
     FOR ALL ENTRIES IN t_zmmt_ee_zgr
   WHERE lifnr = t_zmmt_ee_zgr-lifnr.

  LOOP AT t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr.
    CLEAR wa_zmmt_ee_zgr_aux.

    wa_zmmt_ee_zgr_aux-comp_code = wa_zmmt_ee_zgr-comp_code .

    IF wa_zmmt_ee_zgr-desm_pagos = 'X'.
      wa_zmmt_ee_zgr-desm_pagos = ''.
    ENDIF.

    CONCATENATE 'S' wa_zmmt_ee_zgr-desm_pagos wa_zmmt_ee_zgr-obj_key wa_zmmt_ee_zgr-pstng_date(4) INTO  wa_zmmt_ee_zgr_aux-obj_key.

    APPEND wa_zmmt_ee_zgr_aux TO t_zmmt_ee_zgr_aux.
  ENDLOOP.

  SORT t_zmmt_ee_zgr_aux BY obj_key .
  CLEAR wa_zmmt_ee_zgr.
  SELECT *
    FROM zib_contabil
    INTO TABLE t_zib_contabil
     FOR ALL ENTRIES IN t_zmmt_ee_zgr_aux
   WHERE obj_key = t_zmmt_ee_zgr_aux-obj_key.

  "CS2017002567 - Ini
  SELECT *
    FROM zib_contabil APPENDING TABLE t_zib_contabil
     FOR ALL ENTRIES IN t_zmmt_ee_zgr
   WHERE obj_key = t_zmmt_ee_zgr-obj_key.
  "CS2017002567 - Fim

  IF t_zib_contabil[] IS NOT INITIAL.
    SELECT *
      FROM zib_contabil_chv
      INTO TABLE t_zib_contabil_chv
       FOR ALL ENTRIES IN t_zib_contabil
     WHERE obj_key = t_zib_contabil-obj_key.

    SELECT *
      FROM zib_contabil_err
      INTO TABLE t_zib_contabil_err
       FOR ALL ENTRIES IN t_zib_contabil
     WHERE obj_key = t_zib_contabil-obj_key.
  ENDIF.

  IF t_zmmt_ee_zgr_docs_aux[] IS NOT INITIAL.

    SELECT bukrs
           gjahr
           awkey
           belnr
           waers
           stblg
      FROM bkpf
      INTO TABLE t_bkpf
       FOR ALL ENTRIES IN t_zmmt_ee_zgr_docs_aux
     WHERE awkey  = t_zmmt_ee_zgr_docs_aux-awkey
       AND blart   = 'ZG'.

    IF  p_comp IS NOT INITIAL.
      FREE: t_bkpf.
      SELECT bukrs
             gjahr
             awkey
             belnr
             waers
             stblg
        FROM bkpf
        INTO TABLE t_bkpf
         FOR ALL ENTRIES IN t_zmmt_ee_zgr_docs_aux
       WHERE awkey  = t_zmmt_ee_zgr_docs_aux-awkey
         AND blart   = 'ZG'.
*      AND ( EXISTS ( SELECT * FROM bsak
*                      WHERE bukrs EQ bkpf~bukrs
*                        AND belnr EQ bkpf~belnr
*                        AND gjahr EQ bkpf~gjahr
*     ) ) .
    ENDIF.

    IF  p_n_doc IS NOT INITIAL.
      FREE: t_bkpf.
      SELECT bukrs
             gjahr
             awkey
             belnr
             waers
             stblg
        FROM bkpf
        INTO TABLE t_bkpf
         FOR ALL ENTRIES IN t_zmmt_ee_zgr_docs_aux
       WHERE awkey  = t_zmmt_ee_zgr_docs_aux-awkey
         AND blart   = 'ZG'.
*      AND ( NOT EXISTS ( SELECT * FROM bsak
*                      WHERE bukrs EQ bkpf~bukrs
*                        AND belnr EQ bkpf~belnr
*                        AND gjahr EQ bkpf~gjahr
*     ) ) .
    ENDIF.
  ENDIF.

  "CS2017002567 - Ini
  t_zib_contabil_chv_aux[] = t_zib_contabil_chv[].
  DELETE t_zib_contabil_chv_aux WHERE belnr IS INITIAL.
  IF t_zib_contabil_chv_aux[] IS NOT INITIAL.
    IF p_ambos IS NOT INITIAL.
      SELECT bukrs
             gjahr
             awkey
             belnr
             waers
             stblg
        FROM bkpf APPENDING TABLE t_bkpf
         FOR ALL ENTRIES IN t_zib_contabil_chv_aux
       WHERE bukrs   = t_zib_contabil_chv_aux-bukrs
         AND belnr   = t_zib_contabil_chv_aux-belnr
         AND gjahr   = t_zib_contabil_chv_aux-gjahr
         AND blart   = 'ZG'.
    ENDIF.

    IF p_comp IS NOT INITIAL.
      SELECT bukrs
             gjahr
             awkey
             belnr
             waers
             stblg
        FROM bkpf APPENDING TABLE t_bkpf
         FOR ALL ENTRIES IN t_zib_contabil_chv_aux
       WHERE bukrs   = t_zib_contabil_chv_aux-bukrs
         AND belnr   = t_zib_contabil_chv_aux-belnr
         AND gjahr   = t_zib_contabil_chv_aux-gjahr
         AND blart   = 'ZG'.
*        AND ( EXISTS ( SELECT * FROM bsak
*                        WHERE bukrs EQ bkpf~bukrs
*                          AND belnr EQ bkpf~belnr
*                          AND gjahr EQ bkpf~gjahr
*       ) ) .
    ENDIF.

    IF p_n_doc IS NOT INITIAL.
      SELECT bukrs
             gjahr
             awkey
             belnr
             waers
             stblg
        FROM bkpf APPENDING TABLE t_bkpf
         FOR ALL ENTRIES IN t_zib_contabil_chv_aux
       WHERE bukrs   = t_zib_contabil_chv_aux-bukrs
         AND belnr   = t_zib_contabil_chv_aux-belnr
         AND gjahr   = t_zib_contabil_chv_aux-gjahr
         AND blart   = 'ZG'.
*        AND ( NOT EXISTS ( SELECT * FROM bsak
*                        WHERE bukrs EQ bkpf~bukrs
*                          AND belnr EQ bkpf~belnr
*                          AND gjahr EQ bkpf~gjahr
*       ) ) .
    ENDIF.


    SORT t_bkpf BY belnr bukrs  gjahr.
    DELETE ADJACENT DUPLICATES FROM t_bkpf COMPARING belnr bukrs gjahr.
  ENDIF.
  "CS2017002567 - Fim

  IF t_bkpf[] IS NOT INITIAL.

    SELECT bukrs
           belnr
           gjahr
           hwbas
           hwste
      FROM bset
      INTO TABLE t_bset
      FOR ALL ENTRIES IN t_bkpf
     WHERE bukrs  = t_bkpf-bukrs
       AND belnr  = t_bkpf-belnr
       AND gjahr  = t_bkpf-gjahr.

    SELECT bukrs
           belnr
           gjahr
           witht
           wt_withcd
           wt_qsshh
      FROM with_item
      INTO TABLE t_with_item
       FOR ALL ENTRIES IN t_bkpf
     WHERE bukrs = t_bkpf-bukrs
       AND belnr = t_bkpf-belnr
       AND gjahr = t_bkpf-gjahr
       AND wt_withcd NOT IN ('I4', 'I5', 'I6').

    SELECT setname
           lineid
           valfrom
      FROM setleaf
      INTO TABLE t_setleaf
     WHERE setname = 'MAGGI_ZFIY0017' .

    SELECT setname
           lineid
           descript
      FROM setlinet
      INTO TABLE t_setlinet
     WHERE setname = 'MAGGI_ZFIY0017' .

    SORT : t_setlinet BY setname lineid,
           t_setleaf  BY setname lineid.

    LOOP AT t_setleaf INTO wa_setleaf.

      READ TABLE t_setlinet INTO wa_setlinet WITH KEY setname = wa_setleaf-setname  lineid = wa_setleaf-lineid.

      CLEAR: it_with_cd[].
      IF wa_setlinet-descript IS NOT INITIAL.
        SPLIT wa_setlinet-descript AT ',' INTO TABLE it_with_cd.
      ENDIF.

      LOOP AT it_with_cd.
        DELETE t_with_item WHERE witht = wa_setleaf-valfrom AND wt_withcd =  it_with_cd-wt_withcd.
      ENDLOOP.

    ENDLOOP.
    CLEAR wa_setleaf.

    IF t_with_item[] IS NOT INITIAL.
      SELECT witht
             wt_withcd
             qsatz
        FROM t059z
        INTO TABLE t_t059z
         FOR ALL ENTRIES IN t_with_item
       WHERE land1    = 'AR'
         AND witht     = t_with_item-witht
         AND wt_withcd = t_with_item-wt_withcd .
    ENDIF.

    SELECT augbl
               bukrs
               belnr
               gjahr
          FROM bsak
          INTO TABLE t_bsak
           FOR ALL ENTRIES IN t_bkpf
         WHERE bukrs = t_bkpf-bukrs
           AND belnr = t_bkpf-belnr
           AND gjahr = t_bkpf-gjahr.

*    IF t_bsak IS NOT INITIAL.
*      r_belnr = VALUE #( FOR l IN t_bsak ( sign = 'I' option = 'EQ' low = l-belnr ) ).
*    ENDIF.
*
*    IF p_comp IS NOT INITIAL.
*      IF t_bkpf IS NOT INITIAL.
*        DELETE t_bkpf WHERE belnr NOT IN r_belnr.
*      ENDIF.
*    ENDIF.
*
*    IF p_n_doc IS NOT INITIAL.
*      IF t_bkpf IS NOT INITIAL.
*        DELETE t_bkpf WHERE belnr IN r_belnr.
*      ENDIF.
*    ENDIF.

  ENDIF.

ENDFORM.                    "F_SELECIONA_DADOS



*&---------------------------------------------------------------------*
*&      Form  F_PEGA_TIPO_BANCO_PARCEIRO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*

FORM f_pega_tipo_banco_parceiro.

  IF wa_ekko-lifnr = wa_zmmt_ee_zgr-lifnr. "SE PRODUTOR FORNECEDOR FOR IGUAL A PRODUTOR
    CLEAR: vi001,vt001,vvazi.
    SELECT CASE bvtyp
         WHEN ' ' THEN 'VAZI'
        ELSE bvtyp
       END AS bvtyp FROM lfbk
      INTO CORRESPONDING FIELDS OF TABLE @it_lfbk
    WHERE lifnr = @wa_ekko-lifnr
      GROUP BY lifnr, bvtyp.

    READ TABLE it_lfbk INTO vi001 WITH KEY bvtyp = 'I001'.
    READ TABLE it_lfbk INTO vt001 WITH KEY bvtyp = 'T001'.
    READ TABLE it_lfbk INTO vvazi WITH KEY bvtyp = 'VAZI'.

  ENDIF.
ENDFORM.

FORM f_pega_tipo_banco_parceiro_2.
  PERFORM f_pega_tipo_banco_parceiro.
  IF it_lfbk IS NOT INITIAL.
    IF vi001 = 'I001' . " SE CONTER I001
      wa_zib_contabil-bvtyp = 'I001'.
    ELSEIF vt001 = 'T001'. " SENAO SE CONTER T001
      wa_zib_contabil-bvtyp = 'T001' .
    ELSE.
      wa_zib_contabil-bvtyp = abap_false. "ENTÃO SERÁ VAZIO
    ENDIF.
  ENDIF.
ENDFORM.

FORM f_pega_tipo_banco_parceiro_3.
  PERFORM f_pega_tipo_banco_parceiro.
  IF it_lfbk IS NOT INITIAL.
    IF vvazi = 'VAZI'. "SE CONTER VAZIO
      wa_zib_contabil-bvtyp = space.
    ELSEIF vt001 = 'T001' . " SENAO SE CONTER T001
      wa_zib_contabil-bvtyp = 'T001' .
    ELSE.
      wa_zib_contabil-bvtyp = 'I001' . "SENTÃO SERÁ I001
    ENDIF.
  ENDIF.
ENDFORM.


*&---------------------------------------------------------------------*
*&      Form  F_SAIDA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_saida .

  DATA : vl_awkey      TYPE bkpf-awkey,
         vl_total_prod TYPE with_item-wt_qsshh,
         vl_obj_key    TYPE zib_contabil-obj_key,
         vl_desc       TYPE c LENGTH 1,
         vl_i2_i5      TYPE i,
         vl_valor      TYPE t059z-qsatz. "Descartar Registro


  SORT : t_bkpf             BY awkey,
         t_ekko             BY ebeln,
         t_lfa1_ek          BY lifnr,
         t_lfa1             BY lifnr ,
         "T_LFBW             BY LIFNR WITHT,  BUG 61386
         t_with_item        BY belnr bukrs gjahr,
         t_zib_contabil     BY obj_key,
         t_zib_contabil_chv BY obj_key,
         t_zib_contabil_err BY obj_key,
         t_zmmt_ee_zgr_docs BY obj_key,
       "  T_T059Z            BY WITHT WT_WITHCD, BUG 61386
         t_bset             BY belnr bukrs  gjahr,
         t_ekbe             BY ebeln belnr gjahr,
         t_rbkp             BY belnr gjahr,
         "T_BSAK             BY BUKRS BELNR GJAHR. BUG 61386
.
  REFRESH t_saida.

  LOOP AT t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr.

    CLEAR: wa_zib_contabil_i54, wa_zib_contabil_chv_i54, wa_ekbe, wa_zmmt_ee_zgr_docs,
           wa_rbkp, wa_lfa1,wa_lfa1_ek, wa_ekko, wa_bkpf, wa_bsak, vl_awkey.

    vl_desc = ''.

    wa_saida-contrato = wa_zmmt_ee_zgr-text1.


    "CS2017002567 - Ini
    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      LOOP AT t_zib_contabil INTO wa_zib_contabil_i54 WHERE obj_key = wa_zmmt_ee_zgr-obj_key
                                                        AND ( ( bschl EQ '21' ) OR
                                                              ( bschl EQ '31' ) ).
        EXIT.
      ENDLOOP.
      IF ( wa_zib_contabil_i54 IS NOT INITIAL ).
        READ TABLE t_zib_contabil_chv INTO wa_zib_contabil_chv_i54  WITH KEY obj_key = wa_zib_contabil_i54-obj_key.
      ENDIF.
    ENDIF.
    "CS2017002567 - Fim

    IF wa_zmmt_ee_zgr-desm_pagos = 'X'.
      wa_zmmt_ee_zgr-desm_pagos = ''.
    ENDIF.

    CONCATENATE 'S' wa_zmmt_ee_zgr-desm_pagos wa_zmmt_ee_zgr-obj_key wa_zmmt_ee_zgr-pstng_date(4) INTO vl_obj_key.

    wa_saida-obj_key     = wa_zmmt_ee_zgr-obj_key.
    wa_saida-interface_miro = wa_zmmt_ee_zgr-interface_miro.
    wa_saida-doc_date    = wa_zmmt_ee_zgr-doc_date.
    wa_saida-pstng_date  = wa_zmmt_ee_zgr-pstng_date.
    wa_saida-vr_bruto    = wa_zmmt_ee_zgr-vr_bruto.
    wa_saida-corredor    = wa_zmmt_ee_zgr-lifnr.
    wa_saida-bukrs       = wa_zmmt_ee_zgr-comp_code.
    wa_saida-desm_pagos  = wa_zmmt_ee_zgr-desm_pagos.
    wa_saida-nt_remessa  = wa_zmmt_ee_zgr-nt_remessa.
    wa_saida-tp_operacao = wa_zmmt_ee_zgr-tp_operacao.
    wa_saida-vl_cmv      = wa_zmmt_ee_zgr-vl_cmv.

    READ TABLE t_zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs WITH KEY obj_key = wa_zmmt_ee_zgr-obj_key BINARY SEARCH.

    IF ( sy-subrc IS NOT INITIAL ) AND ( wa_zmmt_ee_zgr-interface_miro NE '54' ). "CS2017002567
      vl_desc = 'X'.
    ENDIF.

    wa_saida-belnr      = wa_zmmt_ee_zgr_docs-ft_belnr.
    wa_saida-gjahr      = wa_zmmt_ee_zgr_docs-ft_gjahr.
    READ TABLE t_rbkp INTO wa_rbkp WITH KEY belnr  = wa_zmmt_ee_zgr_docs-ft_belnr gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr BINARY SEARCH.

    IF ( wa_rbkp-stblg NE '' ) AND ( wa_zmmt_ee_zgr-interface_miro NE '54' ). "CS2017002567
      vl_desc = 'X'.
    ENDIF.

    wa_saida-vencimento = wa_rbkp-zfbdt + wa_rbkp-zbd1t .
    wa_saida-lifnr_miro = wa_rbkp-lifnr.

    READ TABLE t_ekbe INTO wa_ekbe WITH KEY ebeln  = wa_zmmt_ee_zgr-po_number
                                            belnr  = wa_zmmt_ee_zgr_docs-ft_belnr
                                            gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr BINARY SEARCH.

    DATA(_shkzg) = wa_ekbe-shkzg.

    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      IF wa_zib_contabil_i54-bschl = '21'.
        _shkzg = 'S'.
      ELSEIF wa_zib_contabil_i54-bschl = '31'.
        _shkzg = 'H'.
      ENDIF.
    ENDIF.

    wa_saida-po_number = wa_zmmt_ee_zgr-po_number.

    IF ( wa_ekbe-vgabe = 2 ) OR ( wa_zmmt_ee_zgr-interface_miro EQ '54' ). "CS2017002567
      wa_saida-tp_miro = 'Factura'.
    ELSEIF wa_ekbe-vgabe = 3 AND wa_ekbe-shkzg = 'H' .
      wa_saida-tp_miro = 'Abono'.
    ELSEIF wa_ekbe-vgabe = 3 AND wa_ekbe-shkzg = 'S' .
      wa_saida-tp_miro = 'Descargo Posterior'.
    ENDIF.

    CONCATENATE wa_zmmt_ee_zgr_docs-ft_belnr  wa_zmmt_ee_zgr_docs-ft_gjahr INTO vl_awkey  .

    READ TABLE t_bkpf INTO wa_bkpf WITH KEY awkey  = vl_awkey BINARY SEARCH.

    "CS2017002567 - Ini
    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      CLEAR: wa_bkpf.

      READ TABLE t_bkpf INTO wa_bkpf WITH KEY bukrs   = wa_zib_contabil_chv_i54-bukrs
                                              belnr   = wa_zib_contabil_chv_i54-belnr
                                              gjahr   = wa_zib_contabil_chv_i54-gjahr.

      IF ( sy-subrc NE 0      ) OR ( wa_bkpf-stblg IS NOT INITIAL ) OR
         ( wa_bkpf IS INITIAL ) OR ( wa_zib_contabil_chv_i54 IS INITIAL ).
        vl_desc = 'X'.
      ENDIF.

      wa_saida-belnr = wa_zib_contabil_chv_i54-belnr.
    ENDIF.
    "CS2017002567 - Fim

    READ TABLE t_ekko INTO wa_ekko WITH KEY  ebeln  = wa_zmmt_ee_zgr-po_number BINARY SEARCH.

    READ TABLE t_lfa1_ek INTO wa_lfa1_ek WITH KEY lifnr = wa_ekko-lifnr BINARY SEARCH.

    "CS2017002567 - Ini
    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      CLEAR: wa_saida-vencimento, wa_saida-lifnr_miro.

      wa_saida-lifnr_miro = wa_ekko-lifnr.
      IF wa_zib_contabil_i54 IS NOT INITIAL.
        CONCATENATE wa_zib_contabil_i54-zfbdt+6(4) wa_zib_contabil_i54-zfbdt+3(2) wa_zib_contabil_i54-zfbdt(2)
               INTO wa_saida-vencimento.
      ENDIF.
    ENDIF.
    "CS2017002567 - Fim

    wa_saida-waers      = wa_bkpf-waers.
    wa_saida-prod       = wa_lfa1_ek-lifnr.
    wa_saida-nome_prod  = wa_lfa1_ek-name1   .

    READ TABLE t_lfa1 INTO wa_lfa1 WITH KEY lifnr = wa_zmmt_ee_zgr-lifnr BINARY SEARCH.

    wa_saida-nome_corredor = wa_lfa1-name1.
    "wa_saida-valor            type ZIB_CONTABIL-WRBTR  ,
    "WA_SAIDA-ZIB_CONTABIL_CHV = VL_AWKEY.

    vl_total_prod = 0.
    vl_i2_i5      = 0.

    IF ( _shkzg NE 'H' )  AND wa_zmmt_ee_zgr-vl_cmv > 0.

      " LOOP AT T_WITH_ITEM INTO WA_WITH_ITEM WHERE  BELNR = WA_BKPF-BELNR AND BUKRS = WA_BKPF-BUKRS AND GJAHR = WA_BKPF-GJAHR. BUG 61386
      LOOP AT t_with_item INTO wa_with_item USING KEY key_belnr   WHERE  belnr = wa_bkpf-belnr AND bukrs = wa_bkpf-bukrs AND gjahr = wa_bkpf-gjahr.

        READ TABLE t_t059z INTO wa_t059z WITH KEY witht = wa_with_item-witht  wt_withcd = wa_with_item-wt_withcd. " BUG 61386  BINARY SEARCH.
        READ TABLE t_lfbw INTO wa_lfbw WITH KEY lifnr = wa_ekko-lifnr
                                                witht = wa_with_item-witht BINARY SEARCH. "ALRS somentes impostos cadastrados para produtor
        IF sy-subrc = 0.
          vl_total_prod = vl_total_prod + ( ( wa_with_item-wt_qsshh * -1 ) * ( wa_t059z-qsatz / 100 ) ) .
          IF wa_with_item-wt_withcd = 'I2' OR wa_with_item-wt_withcd = 'I5' .
            vl_i2_i5 = vl_i2_i5 + 1.
          ENDIF.
        ENDIF.
      ENDLOOP.

      "      READ TABLE T_BSET INTO WA_BSET WITH KEY BUKRS  = WA_BKPF-BUKRS BELNR  = WA_BKPF-BELNR GJAHR  = WA_BKPF-GJAHR BINARY SEARCH.
      IF vl_i2_i5 = 0 .
        LOOP AT t_bset INTO wa_bset USING KEY key_belnr WHERE bukrs  = wa_bkpf-bukrs AND
                                          belnr  = wa_bkpf-belnr AND
                                          gjahr  = wa_bkpf-gjahr.

          vl_total_prod = vl_total_prod + wa_bset-hwste.

          CLEAR wa_bset.
        ENDLOOP.
      ELSEIF  vl_i2_i5 > 0  AND vl_total_prod > 0.
        vl_valor = '0.01'.
        vl_total_prod = vl_total_prod + vl_valor.
      ENDIF.

    ENDIF.

    wa_saida-total_prod = vl_total_prod .

    wa_saida-valor = wa_saida-vr_bruto - wa_saida-total_prod.

    "CS2017002567 - Ini
    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      ADD wa_saida-total_prod TO wa_saida-valor.
      CLEAR: wa_saida-total_prod.
    ENDIF.
    "CS2017002567 - Fim

    READ TABLE t_zib_contabil INTO wa_zib_contabil WITH KEY obj_key = vl_obj_key BINARY SEARCH.
    IF sy-subrc IS INITIAL .

      wa_saida-status     = icon_activity.
      READ TABLE t_zib_contabil_chv  INTO wa_zib_contabil_chv  WITH KEY obj_key = vl_obj_key BINARY SEARCH.
      IF sy-subrc IS INITIAL.
        wa_saida-status = icon_okay.
        wa_saida-nr_doc = wa_zib_contabil_chv-belnr.

      ELSE.
        READ TABLE t_zib_contabil_err  INTO wa_zib_contabil_err  WITH KEY obj_key = vl_obj_key BINARY SEARCH.
        IF sy-subrc IS INITIAL.
          wa_saida-message = wa_zib_contabil_err-message ."Este valor passar para o ALV coluna Log Erros)
          wa_saida-status  = icon_cancel.
        ENDIF.
      ENDIF.

    ELSEIF wa_zmmt_ee_zgr-zrg_atlz EQ '0'.
      wa_saida-status     = icon_transport.
    ELSE.
      wa_saida-status     = icon_status_open.
    ENDIF.

    IF  _shkzg EQ 'H'.  "CS2017002567
      wa_saida-valor    = wa_saida-valor * -1.
      wa_saida-vr_bruto = wa_saida-vr_bruto * -1.
    ENDIF.

    READ TABLE t_bsak INTO wa_bsak WITH KEY bukrs = wa_bkpf-bukrs belnr = wa_bkpf-belnr	gjahr = wa_bkpf-gjahr .

    IF sy-subrc IS INITIAL .
      wa_saida-augbl = wa_bsak-augbl.
    ELSE.
      wa_saida-augbl = icon_message_warning.
    ENDIF.

    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      IF (  wa_lfa1_ek-lifnr = wa_zmmt_ee_zgr-lifnr ) .
        vl_desc = 'X'.
      ENDIF.

      "Comentado para atender #111095 - CS2023000319 gerar split para pagos directos-SCABANA
    ELSEIF ( wa_rbkp-lifnr = wa_zmmt_ee_zgr-lifnr ).
      "vl_desc = 'X'.
    ENDIF.

    CLEAR : wa_zmmt_ee_zgr,
            wa_zmmt_ee_zgr_docs,
            wa_lfa1_ek,
            wa_lfa1,
            wa_bkpf,
            wa_ekko,
            wa_bsak,
            vl_awkey.

    IF wa_saida-prod NOT IN p_proved AND p_proved IS NOT INITIAL.
      vl_desc = 'X'.
    ENDIF  .

    IF ( p_venc-low IS NOT INITIAL OR p_venc-high IS NOT INITIAL ) AND wa_saida-vencimento NOT IN p_venc.
      vl_desc = 'X'.
    ENDIF  .

    IF ( fc_proc  = 'X' AND wa_saida-status EQ icon_status_open ) OR " Processado
       ( fc_nproc = 'X' AND wa_saida-status NE icon_status_open ).  " Não Processado
      vl_desc = 'X'.
    ENDIF.

    IF vl_desc = 'X'.
      CLEAR wa_saida.
      CONTINUE.
    ENDIF.


    APPEND wa_saida TO t_saida.

    CLEAR : wa_saida.

  ENDLOOP.

  IF p_comp IS NOT INITIAL.
    DELETE t_saida WHERE augbl EQ icon_message_warning.
  ENDIF.

  IF p_n_doc IS NOT INITIAL.
    DELETE t_saida WHERE augbl NE icon_message_warning.
  ENDIF.

ENDFORM.                    " F_SAIDA

*&---------------------------------------------------------------------*
*&      Form  F_ALV
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM f_alv.
  PERFORM alv_preenche_cat USING:
        'STATUS'           TEXT-014      '10'       ' '  'X'     ' ' ,
        'AUGBL'            TEXT-020      '10'       ' '  'X'     ' ' ,
        'TP_MIRO'          TEXT-017      '12'       ' '  'X'     ' ' ,
        'NR_DOC'           TEXT-011      '12'       'X'  'X'     ' ' ,
        'BELNR'            TEXT-013      '12'       'X'  'X'     ' ' ,
        'NT_REMESSA'       TEXT-022      '20'       ' '  ' '     ' ' ,
        'DOC_DATE'         TEXT-012      '12'       ' '  'X'     ' ' ,
        'PSTNG_DATE'       TEXT-002      '13'       ' '  'X'     ' ' ,
        'VENCIMENTO'       TEXT-018      '13'       ' '  'X'     ' ' ,
        'VR_BRUTO'         TEXT-003      '11'       ' '  'X'     ' ' ,
        'WAERS'            TEXT-004      '10'       ' '  'X'     ' ' ,
        'PROD'             TEXT-006      '12'       ' '  'X'     ' ' ,
        'NOME_PROD'        TEXT-005      '29'       ' '  'X'     ' ' ,
        'TOTAL_PROD'       TEXT-007      '12'       ' '  'X'     ' ' ,
        'CORREDOR'         TEXT-008      '12'       ' '  'X'     ' ' ,
        'NOME_CORREDOR'    TEXT-009      '29'       ' '  'X'     ' ' ,
        'VALOR'            TEXT-010      '12'       ' '  'X'     ' ' ,
        'PO_NUMBER'        TEXT-019      '20'       'X'  'X'     ' ' ,
        'CONTRATO'         TEXT-023      '20'       ' '  ' '     ' ' ,
        'MESSAGE'          TEXT-015      '29'       ' '  'X'     ' ' .

ENDFORM.                    " F_ALV
*&---------------------------------------------------------------------*
*&      Module  Z_STATUS  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_status OUTPUT.
  SET PF-STATUS 'FF0100'.
  SET TITLEBAR  'TB0100'.
ENDMODULE.                    "z_status OUTPUT

*&---------------------------------------------------------------------*
*&      Form  ALV_PREENCHE_CAT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_0334   text
*      -->P_TEXT_002  text
*      -->P_0336   text
*      -->P_0337   text
*----------------------------------------------------------------------*
FORM alv_preenche_cat  USING   p_campo TYPE c
                               p_desc  TYPE c
                               p_tam   TYPE c
                               p_hot   TYPE c
                               p_zero  TYPE c
                               p_mask  TYPE c.
  DATA: wl_fcat TYPE lvc_s_fcat.

  wl_fcat-tabname   = 'T_SAIDA'.
  wl_fcat-fieldname = p_campo.
  wl_fcat-scrtext_l = p_desc.
  wl_fcat-scrtext_m = p_desc.
  wl_fcat-scrtext_s = p_desc.
  wl_fcat-hotspot   = p_hot.
  wl_fcat-no_zero   = p_zero.
  wl_fcat-edit_mask = p_mask.
  wl_fcat-outputlen = p_tam.

  "wl_fcat-convexit  = p_mask.
  APPEND wl_fcat TO it_fcat.

ENDFORM.                    " ALV_PREENCHE_CAT

CLASS lcl_event_receiver DEFINITION DEFERRED.

DATA: wa_event       TYPE REF TO  lcl_event_receiver.
*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver DEFINITION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver DEFINITION.

  PUBLIC SECTION.
    METHODS:
      zm_handle_hotspot FOR EVENT hotspot_click OF cl_gui_alv_grid
        IMPORTING e_row_id
                  e_column_id
                  es_row_no,

      zm_handle_toolbar FOR EVENT toolbar OF cl_gui_alv_grid
        IMPORTING
          e_object e_interactive,

      zm_handle_user_command FOR EVENT user_command OF cl_gui_alv_grid
        IMPORTING
          e_ucomm.
ENDCLASS.                    "lcl_event_receiver DEFINITION

*----------------------------------------------------------------------*
*       CLASS lcl_event_receiver IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_event_receiver IMPLEMENTATION.

  METHOD: zm_handle_hotspot.
    PERFORM z_handle_hotspot USING    e_row_id
                                      e_column_id
                                      es_row_no.
  ENDMETHOD.                    "zm_handle_hotspot


  METHOD zm_handle_toolbar.
*   Incluindo Botão ALV
    PERFORM z_handle_toolbar USING e_object
                                   e_interactive.
  ENDMETHOD.                    "zm_handle_toolbar

  METHOD zm_handle_user_command.
*   User Command Botões Incluidos
    PERFORM z_handle_command USING e_ucomm.
  ENDMETHOD.                    "zm_handle_user_command

ENDCLASS.                    "lcl_event_receiver IMPLEMENTATION

*&---------------------------------------------------------------------*
*&      Module  Z_EXIBE_ALV  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_exibe_alv OUTPUT.

  IF wa_cont IS INITIAL.

    CREATE OBJECT wa_cont
      EXPORTING
        container_name              = 'CC_ALV'
      EXCEPTIONS
        cntl_error                  = 1
        cntl_system_error           = 2
        create_error                = 3
        lifetime_error              = 4
        lifetime_dynpro_dynpro_link = 5
        OTHERS                      = 6.
  ENDIF.
  IF wa_alv IS INITIAL AND NOT
    wa_cont IS INITIAL.

    CREATE OBJECT wa_alv
      EXPORTING
        i_parent          = wa_cont
      EXCEPTIONS
        error_cntl_create = 1
        error_cntl_init   = 2
        error_cntl_link   = 3
        error_dp_create   = 4
        OTHERS            = 5.
  ENDIF.

  IF wa_event IS INITIAL.

    CREATE OBJECT wa_event.
    SET HANDLER: wa_event->zm_handle_hotspot FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_toolbar FOR wa_alv.
    SET HANDLER: wa_event->zm_handle_user_command FOR wa_alv.

  ENDIF.

  wa_layout-sel_mode = 'A'.

  CALL METHOD wa_alv->set_table_for_first_display
    EXPORTING
      is_layout                     = wa_layout
      is_variant                    = gs_variant_c
      i_save                        = 'A'
    CHANGING
      it_outtab                     = t_saida
      it_fieldcatalog               = it_fcat
    EXCEPTIONS
      invalid_parameter_combination = 1
      program_error                 = 2
      too_many_lines                = 3
      OTHERS                        = 4.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
  CHECK NOT wa_alv IS INITIAL.
ENDMODULE.                 " Z_EXIBE_ALV  OUTPUT

*&---------------------------------------------------------------------*
*&      Module  Z_USER_COMMAND  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE z_user_command INPUT.
  IF sy-dynnr EQ '0100'.
    CASE sy-ucomm.
      WHEN 'BACK' OR
           'CANC' OR
           'EXIT'  .
        LEAVE TO SCREEN 0. "ELE RETORNA PARA A TELA QUE CHAMOU.
      WHEN 'GERSPLIT' .
        PERFORM zgerar_split.
      WHEN 'RENOVAR'.
        PERFORM zrenovar.
      WHEN 'COMP'.
        PERFORM zcompensar.
      WHEN 'ESTORNAR'.
        PERFORM zestornar.
    ENDCASE.
  ENDIF.
ENDMODULE.                 " Z_USER_COMMAND  INPUT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_HOTSPOT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_ROW_ID  text
*      -->P_E_COLUMN_ID  text
*      -->P_ES_ROW_NO  text
*----------------------------------------------------------------------*
FORM z_handle_hotspot  USING    p_e_row_id    TYPE lvc_s_row
                                p_e_column_id TYPE  lvc_s_col
                                p_es_row_no   TYPE  lvc_s_roid.

  DATA opt TYPE ctu_params.

  CASE p_e_column_id.
    WHEN 'BELNR'.
      READ TABLE t_saida INTO wa_saida INDEX p_e_row_id.
      IF ( wa_saida-belnr IS NOT INITIAL ).
        IF wa_saida-belnr IS NOT INITIAL.

          IF wa_saida-interface_miro = '54'.
            SET PARAMETER ID  'BLN' FIELD wa_saida-belnr.
            SET PARAMETER ID  'BUK' FIELD wa_saida-bukrs.
            SET PARAMETER ID  'GJR' FIELD wa_saida-gjahr.
            CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
          ELSE.
            SET PARAMETER ID  'RBN' FIELD wa_saida-belnr.
            SET PARAMETER ID  'GJR' FIELD wa_saida-gjahr.

            CALL TRANSACTION  'MIR4' AND SKIP FIRST SCREEN.
          ENDIF.
        ENDIF.
      ENDIF.
    WHEN 'NR_DOC'.
      READ TABLE t_saida INTO wa_saida INDEX p_e_row_id.
      IF wa_saida-nr_doc IS NOT INITIAL.
        SET PARAMETER ID  'BLN' FIELD wa_saida-nr_doc.

        SET PARAMETER ID  'BUK' FIELD wa_saida-bukrs.
        SET PARAMETER ID  'GJR' FIELD wa_saida-gjahr.

        CALL TRANSACTION 'FB03' AND SKIP FIRST SCREEN.
      ENDIF.
    WHEN 'PO_NUMBER'.
      IF wa_saida-po_number IS NOT INITIAL.
        READ TABLE t_saida INTO wa_saida INDEX p_e_row_id.

        SET PARAMETER ID 'BES' FIELD wa_saida-po_number .
        CALL TRANSACTION  'ME23N' AND SKIP FIRST SCREEN.
      ENDIF.

  ENDCASE.

ENDFORM.                    " Z_HANDLE_HOTSPOT

*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_TOOLBAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_OBJECT  text
*      -->P_E_INTERACTIVE  text
*----------------------------------------------------------------------*

FORM z_handle_toolbar  USING    p_object  TYPE REF TO cl_alv_event_toolbar_set
                                p_interactive TYPE char1 .

** Constants for button type
  CONSTANTS:
    c_button_normal           TYPE i VALUE 0,
    c_menu_and_default_button TYPE i VALUE 1,
    c_menu                    TYPE i VALUE 2,
    c_separator               TYPE i VALUE 3,
    c_radio_button            TYPE i VALUE 4,
    c_checkbox                TYPE i VALUE 5,
    c_menu_entry              TYPE i VALUE 6.

  DATA sl_toolbar TYPE stb_button.

* Append Seperator
  MOVE c_separator  TO sl_toolbar-butn_type.
  APPEND sl_toolbar TO p_object->mt_toolbar.



ENDFORM.                    " Z_HANDLE_TOOLBAR


*&---------------------------------------------------------------------*
*&      Form  Z_HANDLE_COMMAND
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_E_UCOMM  text
*----------------------------------------------------------------------*
FORM z_handle_command  USING p_ucomm TYPE syucomm       .

  CASE p_ucomm.
    WHEN 'REMESSA'.
*     Gera Remessa
      CALL METHOD wa_alv->refresh_table_display.
  ENDCASE.
ENDFORM.                    " Z_HANDLE_COMMAND

*&---------------------------------------------------------------------*
*&  Include           Z_1BNFE_MONITOR_I04
*&---------------------------------------------------------------------*
MODULE get_selected_rows INPUT.

  CLEAR it_selected_rows.
  CALL METHOD wa_alv->get_selected_rows
    IMPORTING
      et_index_rows = it_selected_rows.

  CLEAR t_saida_selection.
  LOOP AT it_selected_rows INTO wa_selected_rows.
    READ TABLE t_saida
          INTO wa_saida
         INDEX wa_selected_rows-index.
    MOVE-CORRESPONDING wa_saida TO wa_saida_selection.
    APPEND wa_saida_selection TO t_saida_selection.
  ENDLOOP.

ENDMODULE.                 " get_selected_rows  INPUT

*&---------------------------------------------------------------------*
*&      Form  ZGERAR_SPLIT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zgerar_split.

  DATA : it_zib_contabil TYPE TABLE OF zib_contabil.
  DATA : "VL_AWKEY   TYPE BKPF-AWKEY,
    vl_obj_key    TYPE zib_contabil-obj_key,
    vl_reg        TYPE i,
    vl_index      TYPE i,
    vl_bldat      TYPE zib_contabil-bldat,
    vl_budat      TYPE zib_contabil-budat,
    vl_formapgto  TYPE zib_contabil-zlsch,
    vl_desm_pagos TYPE i,
    vl_cont       TYPE i,
    vl_cont_v     TYPE i,
    vl_cont_c     TYPE c LENGTH 1,
    vl_seqitem    TYPE zib_contabil-seqitem.

  DATA: BEGIN OF dynpfields OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfields.

  dynpfields-fieldname = 'ZMMT_EE_ZGR-PSTNG_DATE'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'ZFIY0018'
      dynumb               = '0100'
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      OTHERS               = 10.

  READ TABLE dynpfields WITH KEY fieldname = 'ZMMT_EE_ZGR-PSTNG_DATE'.
  dtvencto = dynpfields-fieldvalue.

  IF formapgto IS INITIAL.
    MESSAGE 'Informar a data de pagamento!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF  bcoempresa IS INITIAL.
    MESSAGE 'Informar a banco de pagamento!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF  dtvencto IS INITIAL.
    MESSAGE 'Informar a data de vencimento!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF  dtbudat IS INITIAL.
    MESSAGE 'Informar a data de contabilização!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF  dtbudat GT sy-datum.
    MESSAGE 'Data de contabilização maior que data atual!' TYPE 'W'.
    EXIT.
  ENDIF.

  t_saida_bloq[] = t_saida[].
  SORT t_saida_bloq BY prod vencimento corredor.
  DELETE ADJACENT DUPLICATES FROM t_saida_bloq COMPARING prod vencimento corredor.

  LOOP AT t_saida_selection INTO wa_saida_selection.

    CLEAR: vl_obj_key,  vl_desm_pagos, wa_zmmt_ee_zgr, wa_ekko, wa_bkpf, wa_rbkp, wa_ekbe, wa_zmmt_ee_zgr_docs.

    REFRESH it_zib_contabil.

    IF ( wa_saida_selection-status  = icon_activity ).
      MESSAGE i001(zfiy) DISPLAY LIKE 'W'.
      CONTINUE.
    ENDIF.

    IF ( wa_saida_selection-status = icon_okay ).
      MESSAGE i002(zfiy) DISPLAY LIKE 'W'.
      CONTINUE.
    ENDIF.

    IF wa_saida_selection-desm_pagos = ''.
      vl_desm_pagos = 0.
    ELSE.
      vl_desm_pagos = wa_saida_selection-desm_pagos .
      vl_desm_pagos = vl_desm_pagos + 1.
    ENDIF.

    wa_saida_selection-desm_pagos = vl_desm_pagos.

    READ TABLE t_saida INTO wa_saida WITH KEY obj_key = wa_saida_selection-obj_key.

    MODIFY t_saida_selection FROM wa_saida.

    READ TABLE t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY obj_key = wa_saida_selection-obj_key.
    READ TABLE t_ekko INTO wa_ekko WITH KEY  ebeln  = wa_zmmt_ee_zgr-po_number .
    READ TABLE t_zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs WITH KEY obj_key = wa_zmmt_ee_zgr-obj_key BINARY SEARCH.
    READ TABLE t_rbkp INTO wa_rbkp WITH KEY belnr  = wa_zmmt_ee_zgr_docs-ft_belnr gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr .
    READ TABLE t_ekbe INTO wa_ekbe WITH KEY ebeln  = wa_zmmt_ee_zgr-po_number
                                            belnr  = wa_zmmt_ee_zgr_docs-ft_belnr
                                            gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr BINARY SEARCH.


    DATA(_shkzg) = wa_ekbe-shkzg.
    DATA(_lifnr) = wa_rbkp-lifnr.

    IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
      _lifnr = wa_ekko-lifnr.
      LOOP AT t_zib_contabil INTO wa_zib_contabil_i54 WHERE obj_key = wa_zmmt_ee_zgr-obj_key
                                                        AND ( ( bschl EQ '21' ) OR
                                                              ( bschl EQ '31' ) ).
        IF wa_zib_contabil_i54-bschl = '21'.
          _shkzg = 'H'.
        ELSEIF wa_zib_contabil_i54-bschl = '31'.
          _shkzg = 'S'.
        ENDIF.
        EXIT.
      ENDLOOP.
    ENDIF.

    CONCATENATE 'S' wa_saida_selection-desm_pagos wa_zmmt_ee_zgr-obj_key wa_zmmt_ee_zgr-pstng_date(4) INTO vl_obj_key .
    IF wa_zmmt_ee_zgr-pstng_date+0(6) EQ sy-datum+0(6).
      CONCATENATE wa_zmmt_ee_zgr-pstng_date+6(2) '.' wa_zmmt_ee_zgr-pstng_date+4(2) '.' wa_zmmt_ee_zgr-pstng_date(4) INTO vl_budat.
    ELSE.
      CONCATENATE dtbudat+6(2) '.' dtbudat+4(2) '.' dtbudat(4) INTO vl_budat.
    ENDIF.

    CONCATENATE wa_zmmt_ee_zgr-doc_date+6(2) '.' wa_zmmt_ee_zgr-doc_date+4(2) '.' wa_zmmt_ee_zgr-doc_date(4) INTO vl_bldat.

    "1 - PARTIDA DA FATURA (MIRO)
    CLEAR wa_zib_contabil.
    vl_cont = 1.
    vl_cont_c = vl_cont.
    CONCATENATE '000' vl_cont_c INTO vl_seqitem.
    wa_zib_contabil-obj_key    = vl_obj_key.
    wa_zib_contabil-seqitem    = vl_seqitem.

    IF _shkzg = 'S' .
      wa_zib_contabil-bschl      = '21'.
    ELSE.
      wa_zib_contabil-bschl      = '31'.
*      IF WA_EKKO-BSART = 'ZGEF'.
*        WA_ZIB_CONTABIL-BSCHL      = '39'.
*        WA_ZIB_CONTABIL-UMSKZ      = 'K'.
*      ENDIF.
    ENDIF.

    IF wa_zmmt_ee_zgr-plant(1) EQ 'A' OR wa_zmmt_ee_zgr-plant(1) EQ 'F'.
      wa_zib_contabil-gsber    = 'T001'.
    ELSE.
      wa_zib_contabil-gsber    = 'F001'.
    ENDIF.

*    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*      EXPORTING
*        INPUT  = WA_ZMMT_EE_ZGR-TEXT1
*      IMPORTING
*        OUTPUT = WA_ZIB_CONTABIL-KIDNO.

    wa_zib_contabil-kidno      = wa_zmmt_ee_zgr-text1+0(30).
    wa_zib_contabil-bukrs      = wa_zmmt_ee_zgr-comp_code.
    wa_zib_contabil-interface  = '30'.
    wa_zib_contabil-bktxt      = wa_saida_selection-belnr.
    wa_zib_contabil-bldat      = vl_bldat.
    wa_zib_contabil-budat      = vl_budat.
    "
    IF wa_zmmt_ee_zgr-pstng_date+0(6) EQ sy-datum+0(6).
      wa_zib_contabil-gjahr      = wa_zmmt_ee_zgr-pstng_date(4).
      wa_zib_contabil-monat      = wa_zmmt_ee_zgr-pstng_date+4(2).
    ELSE.
      wa_zib_contabil-gjahr      = dtbudat(4).
      wa_zib_contabil-monat      = dtbudat+4(2).
    ENDIF.
    "
    wa_zib_contabil-blart      = 'ZY'.
    wa_zib_contabil-xblnr      = wa_zmmt_ee_zgr-nt_remessa.
    wa_zib_contabil-hkont      = _lifnr.
    wa_zib_contabil-wrbtr      = abs( wa_zmmt_ee_zgr-vr_bruto ).
    wa_zib_contabil-waers      = wa_saida_selection-waers.
    "    WA_ZIB_CONTABIL-BUPLA      = WA_ZMMT_EE_ZGR-PLANT.
    wa_zib_contabil-sgtxt      = wa_saida_selection-nome_prod.
    wa_zib_contabil-zuonr      = wa_zmmt_ee_zgr-po_number.
    wa_zib_contabil-rg_atualizado  = 'N'.
    wa_zib_contabil-zlsch     = formapgto.
    wa_zib_contabil-hbkid     = bcoempresa.
    wa_zib_contabil-zfbdt     = dtvencto.
    APPEND wa_zib_contabil TO it_zib_contabil.
    "-------------------------------------------------------------

    "2 – ABERTURA PRODUTOR
    IF _shkzg NE 'H' AND wa_saida_selection-total_prod > 0 AND
      " wa_saida_selection-tp_operacao NE '03' AND  -retirado a pedido do marcos Santos dia 15/02/13
       wa_saida_selection-tp_operacao NE '04' AND
       wa_saida_selection-tp_operacao NE '99' AND
       wa_saida_selection-vl_cmv > 0.

      vl_cont_v = 0. "verifica se há mais de um corredor para o mesmo produtor no vencimento
      LOOP AT t_saida_bloq INTO wa_saida_l WHERE prod       = wa_saida_selection-prod
                                           AND   vencimento = wa_saida_selection-vencimento.
        ADD 1 TO vl_cont_v.
      ENDLOOP.
      vl_cont = vl_cont + 1.
      vl_cont_c = vl_cont.
      CONCATENATE '000' vl_cont_c INTO vl_seqitem.

      CLEAR wa_zib_contabil.
      wa_zib_contabil-obj_key    = vl_obj_key.
      wa_zib_contabil-seqitem    = vl_seqitem.
      wa_zib_contabil-bschl      = '31'.

*      IF WA_EKKO-BSART = 'ZGEF'.
*        WA_ZIB_CONTABIL-BSCHL      = '39'.
*        WA_ZIB_CONTABIL-UMSKZ      = 'K'.
*      ENDIF.

      IF wa_zmmt_ee_zgr-plant(1) EQ 'A' OR wa_zmmt_ee_zgr-plant(1) EQ 'F'.
        wa_zib_contabil-gsber = 'T001'.
      ELSE.
        wa_zib_contabil-gsber = 'F001'.
      ENDIF.

      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
        EXPORTING
          input  = wa_zmmt_ee_zgr-text1
        IMPORTING
          output = wa_zib_contabil-kidno.

      wa_zib_contabil-bukrs      = wa_zmmt_ee_zgr-comp_code.
      wa_zib_contabil-interface  = '30'.
      wa_zib_contabil-bktxt      = wa_saida_selection-belnr.
      wa_zib_contabil-bldat      = vl_bldat.
      wa_zib_contabil-budat      = vl_budat.
      "
      IF wa_zmmt_ee_zgr-pstng_date+0(6) EQ sy-datum+0(6).
        wa_zib_contabil-gjahr      = wa_zmmt_ee_zgr-pstng_date(4).
        wa_zib_contabil-monat      = wa_zmmt_ee_zgr-pstng_date+4(2).
      ELSE.
        wa_zib_contabil-gjahr      = dtbudat(4).
        wa_zib_contabil-monat      = dtbudat+4(2).
      ENDIF.
      "
      wa_zib_contabil-blart      = 'ZY'.
      wa_zib_contabil-xblnr      = wa_zmmt_ee_zgr-nt_remessa.
      wa_zib_contabil-hkont      = wa_ekko-lifnr.
      wa_zib_contabil-wrbtr      = abs( wa_saida_selection-total_prod )." VARIAVEL XTOTAL_PROD
      wa_zib_contabil-waers      = wa_saida_selection-waers.
      wa_zib_contabil-sgtxt      = wa_saida_selection-nome_prod.
      wa_zib_contabil-zuonr      = wa_zmmt_ee_zgr-po_number.
      wa_zib_contabil-rg_atualizado  = 'N'.
      wa_zib_contabil-zlsch     = formapgto.
      wa_zib_contabil-hbkid     = bcoempresa.
      wa_zib_contabil-zfbdt     = dtvencto.
      IF vl_cont_v GT 1.
        wa_zib_contabil-zlspr = 'A'.
      ELSE.
        CLEAR wa_zib_contabil-zlspr.
      ENDIF.

**********************************************************************
*  111095 CS2023000319 gerar split para pagos directos - PSA
**********************************************************************

**********************************************************************
* Busca dados na LFBK = LIFNR = ZMMT_EE_ZGR-LIFNR PARTE 2
**********************************************************************

      PERFORM f_pega_tipo_banco_parceiro_2.


**********************************************************************
      APPEND wa_zib_contabil TO it_zib_contabil.
    ENDIF.
**********************************************************************

    "3 – ABERTURA CORREDOR
    CLEAR wa_zib_contabil.
    vl_cont = vl_cont + 1.
    vl_cont_c = vl_cont.
    CONCATENATE '000' vl_cont_c INTO vl_seqitem.

    wa_zib_contabil-obj_key    = vl_obj_key.

    wa_zib_contabil-seqitem    = vl_seqitem.

    IF _shkzg = 'S' .
      wa_zib_contabil-bschl      = '31'.
*      IF WA_EKKO-BSART = 'ZGEF'.
*        WA_ZIB_CONTABIL-BSCHL      = '39'.
*        WA_ZIB_CONTABIL-UMSKZ      = 'K'.
*      ENDIF.
    ELSE.
      wa_zib_contabil-bschl      = '21'.
    ENDIF.

    IF wa_zmmt_ee_zgr-plant(1) EQ 'A' OR wa_zmmt_ee_zgr-plant(1) EQ 'F'.
      wa_zib_contabil-gsber = 'T001'.
    ELSE.
      wa_zib_contabil-gsber = 'F001'.
    ENDIF.

    CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
      EXPORTING
        input  = wa_zmmt_ee_zgr-text1
      IMPORTING
        output = wa_zib_contabil-kidno.

    wa_zib_contabil-bukrs      = wa_zmmt_ee_zgr-comp_code.
    wa_zib_contabil-interface  = '30'.
    wa_zib_contabil-bktxt      = wa_saida_selection-belnr.
    wa_zib_contabil-bldat      = vl_bldat.
    wa_zib_contabil-budat      = vl_budat.
    IF wa_zmmt_ee_zgr-pstng_date+0(6) EQ sy-datum+0(6).
      wa_zib_contabil-gjahr      = wa_zmmt_ee_zgr-pstng_date(4).
      wa_zib_contabil-monat      = wa_zmmt_ee_zgr-pstng_date+4(2).
    ELSE.
      wa_zib_contabil-gjahr      = dtbudat(4).
      wa_zib_contabil-monat      = dtbudat+4(2).
    ENDIF.
    wa_zib_contabil-blart      = 'ZY'.
    wa_zib_contabil-xblnr      = wa_zmmt_ee_zgr-nt_remessa.
    wa_zib_contabil-hkont      = wa_zmmt_ee_zgr-lifnr.
    wa_zib_contabil-wrbtr      = abs( wa_saida_selection-valor ). "WA_ZMMT_EE_ZGR-VR_BRUTO-VARIAVEL XTOTAL_PROD.
    wa_zib_contabil-waers      = wa_saida_selection-waers.
    "    WA_ZIB_CONTABIL-BUPLA      = WA_ZMMT_EE_ZGR-PLANT.
    wa_zib_contabil-sgtxt      = wa_saida_selection-nome_corredor.
    wa_zib_contabil-zuonr      = wa_zmmt_ee_zgr-po_number.
    wa_zib_contabil-rg_atualizado  = 'N'.

    wa_zib_contabil-zlsch     = formapgto.
    wa_zib_contabil-hbkid     = bcoempresa.
    wa_zib_contabil-zfbdt     = dtvencto.
**********************************************************************
*  111095 CS2023000319 gerar split para pagos directos - PSA
**********************************************************************

**********************************************************************
* Busca dados na LFBK = LIFNR = ZMMT_EE_ZGR-LIFNR PARTE 3
**********************************************************************

    PERFORM f_pega_tipo_banco_parceiro_3.

**********************************************************************

    APPEND wa_zib_contabil TO it_zib_contabil.
    "-------------------------------------------------------------
    " WA_SAIDA_SELECTION-STATUS = ICON_ACTIVITY.

    MODIFY zib_contabil FROM TABLE it_zib_contabil.

*    VL_INDEX = 1.
*    LOOP AT T_SAIDA INTO WA_SAIDA.
*      IF WA_SAIDA-OBJ_KEY = WA_SAIDA_SELECTION-OBJ_KEY.
*        MODIFY T_SAIDA FROM WA_SAIDA_SELECTION INDEX VL_INDEX TRANSPORTING STATUS.
*      ENDIF.
*      VL_INDEX = VL_INDEX + 1.
*    ENDLOOP.

    UPDATE zmmt_ee_zgr SET desm_pagos = wa_saida_selection-desm_pagos WHERE obj_key =  wa_saida-obj_key .
    COMMIT WORK.
  ENDLOOP.

  PERFORM zrenovar.

ENDFORM.                    "ZGERAR_SPLIT
*&---------------------------------------------------------------------*
*&      Form  ZRENOVAR
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM zrenovar .

  DATA: vl_obj_key TYPE zib_contabil-obj_key,
        vl_indice  TYPE i.

  vl_indice = 1.

  PERFORM : f_seleciona_dados USING 'X',
            f_saida.

*  LOOP AT T_SAIDA INTO WA_SAIDA .
*
*    IF WA_SAIDA-STATUS  = ICON_OKAY AND WA_SAIDA-DESM_PAGOS EQ ''.
*      UPDATE ZMMT_EE_ZGR SET DESM_PAGOS = '0' WHERE OBJ_KEY =  WA_SAIDA-OBJ_KEY .
*
*      WA_SAIDA-DESM_PAGOS = '0'.
*      MODIFY T_SAIDA FROM WA_SAIDA INDEX VL_INDICE TRANSPORTING  DESM_PAGOS.
*
*      COMMIT WORK.
*    ENDIF.
*    VL_INDICE = VL_INDICE + 1.
*
*  ENDLOOP.


ENDFORM.                    " ZRENOVAR


*&---------------------------------------------------------------------*
*&      Form  zCompensar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zcompensar.
  DATA : vl_mode     TYPE c LENGTH 1,
         vl_ok       TYPE c LENGTH 1,
         vl_data     TYPE zib_contabil-budat,
         vl_vr_bruto TYPE zib_contabil-wrbtr,
         v_vr_bruto  TYPE c LENGTH 20,
         vl_message  TYPE string,
         vl_monat    TYPE bkpf-monat.



  IF  dtbudat IS INITIAL.
    MESSAGE 'Informar a data de contabilização!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF  dtbudat GT sy-datum.
    MESSAGE 'Data de contabilização maior que data atual!' TYPE 'W'.
    EXIT.
  ENDIF.

  IF proc_v EQ 'X'.
    vl_mode = 'A'.
  ELSE.
    vl_mode = 'N'.
  ENDIF.

  LOOP AT t_saida_selection INTO wa_saida_selection.

    IF wa_saida_selection-status  = icon_okay.
      REFRESH ti_bdc.

      CLEAR : vl_data, wa_zmmt_ee_zgr, wa_zmmt_ee_zgr_docs, wa_rbkp, wa_ekko, wa_lfa1_ek.

      READ TABLE t_zmmt_ee_zgr INTO wa_zmmt_ee_zgr WITH KEY obj_key = wa_saida_selection-obj_key .

      READ TABLE t_zmmt_ee_zgr_docs INTO wa_zmmt_ee_zgr_docs WITH KEY obj_key = wa_zmmt_ee_zgr-obj_key .

      READ TABLE t_rbkp INTO wa_rbkp WITH KEY belnr  = wa_zmmt_ee_zgr_docs-ft_belnr gjahr  = wa_zmmt_ee_zgr_docs-ft_gjahr .

      READ TABLE t_ekko INTO wa_ekko WITH KEY  ebeln  = wa_zmmt_ee_zgr-po_number BINARY SEARCH.

      DATA(_lifnr) = wa_rbkp-lifnr.
      IF ( wa_zmmt_ee_zgr-interface_miro EQ '54' ).
        _lifnr = wa_ekko-lifnr.
      ENDIF.

      IF wa_zmmt_ee_zgr-pstng_date+0(6) EQ sy-datum+0(6).
        CONCATENATE wa_zmmt_ee_zgr-pstng_date+6(2) '.' wa_zmmt_ee_zgr-pstng_date+4(2) '.' wa_zmmt_ee_zgr-pstng_date(4) INTO vl_data.
        vl_monat = wa_zmmt_ee_zgr-pstng_date+4(2).
      ELSE.
        CONCATENATE dtbudat+6(2) '.' dtbudat+4(2) '.' dtbudat(4) INTO vl_data.
        vl_monat = dtbudat+4(2).
      ENDIF.

      vl_vr_bruto = wa_zmmt_ee_zgr-vr_bruto.
      v_vr_bruto  = vl_vr_bruto.
      REPLACE '.' WITH ',' INTO v_vr_bruto.
      CONDENSE v_vr_bruto NO-GAPS.

      PERFORM zf_bdc USING: 'X' 'SAPMF05A'         '0131' ,
                            ' ' 'BDC_CURSOR'       'RF05A-XPOS1(02)',
                            ' ' 'BDC_OKCODE'       '=PA',
                            ' ' 'RF05A-AGKON'      _lifnr,    "(RBKP-LIFNR)
                            ' ' 'BKPF-BUDAT'       vl_data,"(ZMMT_EE_ZGR-PSTNG_DATE)
                            ' ' 'BKPF-MONAT'       vl_monat,  "pegar o mês do campo (ZMMT_EE_ZGR-PSTNG_DATE)
                            ' ' 'BKPF-BUKRS'       wa_zmmt_ee_zgr-comp_code, "(ZMMT_EE_ZGR-COMP_CODE)
                            ' ' 'BKPF-WAERS'       'ARS',
                            ' ' 'RF05A-XNOPS'      'X',
                            ' ' 'RF05A-XPOS1(01)'  '' ,
                            ' ' 'RF05A-XPOS1(02)'  'X',

                            'X' 'SAPMF05A'         '0730',
                            ' ' 'BDC_CURSOR'       'RF05A-VONWT(01)',
                            ' ' 'BDC_OKCODE'       '=PA',
                            ' ' 'RF05A-VONWT(01)'   v_vr_bruto,

                            'X' 'SAPDF05X'         '3100',
                            ' ' 'BDC_OKCODE'       '=BU',
                            ' ' 'BDC_SUBSCR'       'SAPDF05X                                6102PAGE',"                                6102PAGE
                            ' ' 'BDC_CURSOR'       'DF05B-PSBET(02)',
                            ' ' 'RF05A-ABPOS'      '1' .


      CALL TRANSACTION 'F-44'
         USING ti_bdc
          MODE vl_mode
        UPDATE 'S'
      MESSAGES INTO ti_msg.

      IF ti_msg[] IS NOT INITIAL.

        LOOP AT ti_msg INTO wa_msg.

          IF wa_msg-msgtyp = 'E'.

            MESSAGE ID wa_msg-msgid
                  TYPE wa_msg-msgtyp
                NUMBER wa_msg-msgnr
                  WITH wa_msg-msgv1 wa_msg-msgv2 wa_msg-msgv3 wa_msg-msgv4
               INTO vl_message.

            MESSAGE vl_message TYPE 'I'.

          ENDIF.

        ENDLOOP.

        COMMIT WORK.
      ELSE.
        vl_ok = 'X'.
      ENDIF.

    ENDIF.

  ENDLOOP.

  IF  vl_ok = 'X'.
    MESSAGE i000(z04) WITH ' Algunos documentos no generó Split'.
  ENDIF.

ENDFORM.                    "zCompensar

*&---------------------------------------------------------------------*
*&      Form  zestornar
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM zestornar.

  DATA: vl_obj_key TYPE zib_contabil-obj_key,
        vl_mode    TYPE c LENGTH 1,
        vl_erro    TYPE c LENGTH 1,
        vl_ok      TYPE c LENGTH 1.

  IF proc_v EQ 'X'.
    vl_mode = 'A'.
  ELSE.
    vl_mode = 'N'.
  ENDIF.

  vl_erro = ''.

  LOOP AT t_saida_selection INTO wa_saida_selection.
    REFRESH ti_bdc.

    IF wa_saida_selection-augbl <> icon_message_warning.
      MESSAGE i000(z04) WITH 'Para invertir debe cancelar la transacción ' 'de compensación FBRA'.
      EXIT.
    ENDIF.

    IF wa_saida_selection-status  = icon_okay.
      IF wa_saida_selection-desm_pagos = ''.
        MESSAGE i000(z04) WITH 'No tenemos documentos para invertir.'.
      ELSE.
        CONCATENATE 'S' wa_saida_selection-desm_pagos wa_saida_selection-obj_key wa_saida_selection-pstng_date(4) INTO vl_obj_key.

        READ TABLE t_zib_contabil_chv INTO wa_zib_contabil_chv WITH KEY obj_key = vl_obj_key BINARY SEARCH.

        PERFORM zf_bdc USING: 'X' 'SAPMF05A'    '0105',
                              ' ' 'BDC_CURSOR'  'UF05A-STGRD',
                              ' ' 'BDC_OKCODE'  '=BU',
                              ' ' 'RF05A-BELNS'	wa_zib_contabil_chv-belnr,
                              ' ' 'BKPF-BUKRS'  wa_zib_contabil_chv-bukrs,
                              ' ' 'RF05A-GJAHS'	wa_zib_contabil_chv-gjahr,
                              ' ' 'UF05A-STGRD'	'01'.

        CALL TRANSACTION 'FB08'
          USING ti_bdc
           MODE vl_mode
         UPDATE 'S'
       MESSAGES INTO ti_msg.

        LOOP AT ti_msg INTO wa_msg.

          IF wa_msg-msgtyp = 'E'.

            vl_erro = 'X'.

          ENDIF.

        ENDLOOP.

        IF vl_erro = ''.

          DELETE FROM zib_contabil     WHERE obj_key = vl_obj_key.
          DELETE FROM zib_contabil_chv WHERE obj_key = vl_obj_key.

        ENDIF.

        COMMIT WORK.

      ENDIF.
    ELSE.
      vl_ok = 'X'.
    ENDIF.

  ENDLOOP.

  IF vl_ok = 'X'.
    MESSAGE i000(z04) WITH ' Algunos documentos no generó Split'.
  ENDIF.

ENDFORM.                    "zestornar

*&---------------------------------------------------------------------*
*&      Form  zf_bdc
*&---------------------------------------------------------------------*
FORM zf_bdc USING p_dynbegin TYPE any
                  p_name     TYPE any
                  p_value    TYPE any.

  IF p_dynbegin EQ 'X'.
    wa_bdc-program  = p_name.
    wa_bdc-dynpro   = p_value.
    wa_bdc-dynbegin = p_dynbegin.

    APPEND wa_bdc
      TO ti_bdc.
  ELSE.
    wa_bdc-fnam = p_name.
    wa_bdc-fval = p_value.

    APPEND wa_bdc
      TO ti_bdc.
  ENDIF.

  CLEAR wa_bdc.
ENDFORM.                    " ZF_BDC

*&---------------------------------------------------------------------*
*&      Module  F_BUSCA_FORMAPGTO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_busca_formapgto INPUT.

  DATA: ti_help      TYPE STANDARD TABLE OF ty_t042z INITIAL SIZE 0 WITH HEADER LINE,
        t_ret        TYPE TABLE OF ddshretval,
        t_dynpfields TYPE STANDARD TABLE OF dynpread INITIAL SIZE 1 WITH HEADER LINE.

  DATA: lo_jurisdiction TYPE REF TO cl_tax_jurisdiction_code,
        vg_langu        TYPE sy-langu,
        st_ret          TYPE ddshretval.

  DATA: BEGIN OF dynpfields OCCURS 2.
          INCLUDE STRUCTURE dynpread.
  DATA: END   OF dynpfields.

  dynpfields-fieldname = 'FORMAPGTO'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'ZFIY0018'
      dynumb               = '0100'
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      OTHERS               = 10.

  READ TABLE dynpfields WITH KEY fieldname = 'FORMAPGTO'.

  SELECT land1
         zlsch
         text1
    INTO CORRESPONDING FIELDS OF TABLE ti_help
    FROM t042z
   WHERE land1 = 'AR'.

  SORT ti_help[] BY land1.

  CHECK NOT ti_help[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'ZLSCH'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help WITH KEY zlsch = st_ret-fieldval BINARY SEARCH.

  MOVE: 'FORMAPGTO'   TO t_dynpfields-fieldname,
        ti_help-zlsch TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.


  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      input  = sy-langu
    IMPORTING
      output = vg_langu.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.

ENDMODULE.                 " F_BUSCA_FORMAPGTO  INPUT
*&---------------------------------------------------------------------*
*&      Module  F_BUSCA_BCOEMPRESA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE f_busca_bcoempresa INPUT.

  DATA: ti_help1  TYPE STANDARD TABLE OF ty_t012  INITIAL SIZE 0 WITH HEADER LINE.

  dynpfields-fieldname = 'BCOEMPRESA'.
  APPEND dynpfields.

  CALL FUNCTION 'DYNP_VALUES_READ'
    EXPORTING
      dyname               = 'ZFIY0018'
      dynumb               = '0100'
    TABLES
      dynpfields           = dynpfields
    EXCEPTIONS
      invalid_abapworkarea = 1
      invalid_dynprofield  = 2
      invalid_dynproname   = 3
      invalid_dynpronummer = 4
      invalid_request      = 5
      no_fielddescription  = 6
      invalid_parameter    = 7
      undefind_error       = 8
      double_conversion    = 9
      OTHERS               = 10.

  READ TABLE dynpfields WITH KEY fieldname = 'BCOEMPRESA'.

  SELECT hbkid
         banks
         bankl
    INTO CORRESPONDING FIELDS OF TABLE ti_help1
    FROM t012
   WHERE bukrs =  '0100'.

  SORT ti_help1[] BY hbkid.

  CHECK NOT ti_help1[] IS INITIAL.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      retfield   = 'HBKID'
      dynpprog   = sy-repid
      dynpnr     = sy-dynnr
      value_org  = 'S'
    TABLES
      value_tab  = ti_help1[]
      return_tab = t_ret.

  READ TABLE t_ret INTO st_ret INDEX 1.
  CHECK sy-subrc IS INITIAL.

  READ TABLE ti_help1 WITH KEY hbkid = st_ret-fieldval BINARY SEARCH.

  MOVE: 'BCOEMPRESA'  TO t_dynpfields-fieldname,
        ti_help1-hbkid TO t_dynpfields-fieldvalue.
  APPEND t_dynpfields.


  CALL FUNCTION 'CONVERSION_EXIT_ISOLA_OUTPUT'
    EXPORTING
      input  = sy-langu
    IMPORTING
      output = vg_langu.

  CALL FUNCTION 'DYNP_VALUES_UPDATE'
    EXPORTING
      dyname     = sy-repid
      dynumb     = sy-dynnr
    TABLES
      dynpfields = t_dynpfields.
ENDMODULE.                 " F_BUSCA_BCOEMPRESA  INPUT
