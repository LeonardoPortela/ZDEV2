FUNCTION z_01_dre_proc_diario.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(PDATA) TYPE  DATUM
*"     REFERENCE(PDATA_FIM) TYPE  DATUM OPTIONAL
*"     REFERENCE(PLIMPAR_TUDO) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(PCARGA) TYPE  CHAR01 OPTIONAL
*"     REFERENCE(P_BUKRS) TYPE  BUKRS OPTIONAL
*"  EXPORTING
*"     REFERENCE(PARCIAL) TYPE  CHAR01
*"  TABLES
*"      IT_LANC STRUCTURE  ZGLT_DRE_04 OPTIONAL
*"  EXCEPTIONS
*"      ERRO_SQL
*"----------------------------------------------------------------------

  DATA: wa_lanc       TYPE zglt_dre_04,
*        WA_RES03      TYPE ZGLT_DRE_03,
        lc_aux        TYPE c LENGTH 16,
        lc_data_ini   TYPE faglflexa-timestamp,
        lc_data_fim   TYPE faglflexa-timestamp,
        it_integrados TYPE TABLE OF zglt_dre_04 WITH HEADER LINE,
        it_lanc_aux   TYPE TABLE OF zglt_dre_04 WITH HEADER LINE,
*        IT_RES03G     TYPE TABLE OF ZGLT_DRE_03 WITH HEADER LINE,
        it_bseg       TYPE TABLE OF bseg WITH HEADER LINE,
        it_mara       TYPE TABLE OF mara WITH HEADER LINE,
        it_lanc_2     TYPE TABLE OF zglt_dre_04,
        wa_lanc_2     TYPE zglt_dre_04,
        parcial_2     TYPE char01.

  CONCATENATE pdata '000000' INTO lc_aux.
  MOVE lc_aux TO lc_data_ini.

  IF pdata_fim IS INITIAL.
    CONCATENATE pdata '999999' INTO lc_aux.
    MOVE lc_aux TO lc_data_fim.
  ELSE.
    CONCATENATE pdata_fim '999999' INTO lc_aux.
    MOVE lc_aux TO lc_data_fim.
  ENDIF.

  CLEAR: it_lanc[],
         it_lanc_aux[].

  IF plimpar_tudo IS NOT INITIAL AND plimpar_tudo EQ 'D'.
    CALL FUNCTION 'Z_01_DRE_LIMPAR'.
  ENDIF.

  IF pcarga IS INITIAL.

    CALL FUNCTION 'Z_01_DRE_SALDOS_RETIRA'
      EXPORTING
        lc_data_ini = lc_data_ini
        lc_data_fim = lc_data_fim
        p_bukrs     = p_bukrs
      EXCEPTIONS
        erro_sql    = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
    ENDIF.

  ENDIF.

  IF p_bukrs IS INITIAL OR
     p_bukrs EQ '0004' OR
     p_bukrs EQ '0022' OR
     p_bukrs EQ '0002' OR
     p_bukrs EQ '0027' OR
     p_bukrs EQ '0001' OR
     p_bukrs EQ '0030' OR
     p_bukrs EQ '0034' OR
     p_bukrs EQ '0025' OR
     p_bukrs EQ '0028' OR
     p_bukrs EQ '0029' OR
     p_bukrs EQ '0024' OR
     p_bukrs EQ '0036' OR
     p_bukrs EQ '0018' OR
     p_bukrs EQ '0014' OR
     p_bukrs EQ '0015' OR
     p_bukrs EQ '0010' OR
     p_bukrs EQ '0026' OR
     p_bukrs EQ '0017' OR
     p_bukrs EQ '0023' OR
     p_bukrs EQ '0032' OR
     p_bukrs EQ '0037' OR
     p_bukrs EQ '0035' OR
     p_bukrs EQ '0038' OR
     p_bukrs EQ '0100' OR
     p_bukrs EQ '0005' OR
     p_bukrs EQ '0020' OR
     p_bukrs EQ '0040' OR
     p_bukrs EQ '0042' OR
     p_bukrs EQ '0200' OR
     p_bukrs EQ '0201' OR
     p_bukrs EQ '0202' OR
     p_bukrs EQ '0044' OR
     p_bukrs EQ '0045' OR
     p_bukrs EQ '0046' OR
     p_bukrs EQ '0047' OR
     p_bukrs EQ '0203' OR

     p_bukrs EQ '0050' OR
     p_bukrs EQ '0051' OR
     p_bukrs EQ '0052' OR
     p_bukrs EQ '0053' OR
     p_bukrs EQ '0054' OR
     p_bukrs EQ '0055' OR
     p_bukrs EQ '0056' OR
     p_bukrs EQ '0048' OR
     p_bukrs EQ '0021' OR  "*-Equalização RISE x PRD - 21.08.2023 - JT
     p_bukrs EQ '0013' OR    "*-Equalização RISE x PRD - 21.08.2023 - JT
     p_bukrs EQ '0072'. "127477 - Incluir a nova empresa 0072 - SMC


    CALL FUNCTION 'Z_01_DRE_CONSULTA_REGS'
      EXPORTING
        lc_data_ini = lc_data_ini
        lc_data_fim = lc_data_fim
        pcarga      = pcarga
        p_bukrs     = p_bukrs
      IMPORTING
        parcial     = parcial
      TABLES
        it_lanc     = it_lanc
      EXCEPTIONS
        erro_sql    = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
    ENDIF.
  ENDIF.

  IF p_bukrs IS INITIAL OR
     p_bukrs EQ '0041'  OR
     p_bukrs EQ '0039'  OR
     p_bukrs EQ '0101'.
    CALL FUNCTION 'Z_01_DRE_CONSULTA_REGS_2'
      EXPORTING
        lc_data_ini = lc_data_ini
        lc_data_fim = lc_data_fim
        pcarga      = pcarga
        p_bukrs     = p_bukrs
      IMPORTING
        parcial     = parcial_2
      TABLES
        it_lanc     = it_lanc_2
      EXCEPTIONS
        erro_sql    = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
    ENDIF.

    IF parcial_2 EQ abap_true.
      parcial = abap_true.
    ENDIF.

    LOOP AT it_lanc_2 INTO wa_lanc_2.
      APPEND wa_lanc_2 TO it_lanc.
    ENDLOOP.
  ENDIF.

  MOVE it_lanc[] TO it_lanc_aux[].

  SORT it_lanc BY bukrs gjahr belnr buzei.

  DELETE it_lanc_aux WHERE matnr NE space.

  IF it_lanc_aux[] IS NOT INITIAL.
    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
        it_for_all_entries = it_lanc_aux[]
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
      IMPORTING
        et_bseg            = it_bseg[]
      EXCEPTIONS
        not_found          = 1.
    IF sy-subrc = 0 AND lines( it_bseg[] ) > 0.
      MOVE-CORRESPONDING it_bseg[] TO it_bseg[].
      sy-dbcnt = lines( it_bseg[] ).
    ELSE.
      sy-subrc = 4.
      sy-dbcnt = 0.
    ENDIF.

  ENDIF.

  DELETE it_bseg WHERE matnr EQ space.

  IF it_bseg[] IS NOT INITIAL.
    SELECT * INTO TABLE it_mara
      FROM mara
       FOR ALL ENTRIES IN it_bseg
     WHERE matnr EQ it_bseg-matnr.

    SORT it_mara BY matnr.
  ENDIF.

  LOOP AT it_bseg.

    READ TABLE it_mara WITH KEY matnr = it_bseg-matnr BINARY SEARCH.

    READ TABLE it_lanc
    WITH KEY bukrs = it_bseg-bukrs
             gjahr = it_bseg-gjahr
             belnr = it_bseg-belnr
             buzei = it_bseg-buzei
             BINARY SEARCH.
    IF sy-subrc IS INITIAL.
      IF it_lanc-matnr IS INITIAL.
        it_lanc-matnr = it_bseg-matnr.
      ENDIF.
      IF it_lanc-matkl IS INITIAL.
        it_lanc-matkl = it_mara-matkl.
      ENDIF.
      MODIFY it_lanc INDEX sy-tabix TRANSPORTING matnr matkl.
    ENDIF.
  ENDLOOP.

  MODIFY zglt_dre_04 FROM TABLE it_lanc.
  COMMIT WORK.

  IF pcarga IS INITIAL.

    CALL FUNCTION 'Z_01_DRE_SALDOS_INCLUIR'
      EXPORTING
        lc_data_ini = lc_data_ini
        lc_data_fim = lc_data_fim
        p_bukrs     = p_bukrs
      TABLES
        it_dre04    = it_lanc
      EXCEPTIONS
        erro_sql    = 1
        OTHERS      = 2.

    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4 RAISING erro_sql.
    ENDIF.

  ENDIF.

ENDFUNCTION.
