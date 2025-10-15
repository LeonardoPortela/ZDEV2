FUNCTION z_fi_gl_part_aberto.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(I_COMPANY) TYPE  BUKRS
*"     REFERENCE(I_MOEDA_DOC) TYPE  WAERS OPTIONAL
*"     REFERENCE(I_FORNE) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_CLIENTE) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_PARID) TYPE  J_1BPARID OPTIONAL
*"     REFERENCE(I_DATA_VENC_INI) TYPE  DZFBDT OPTIONAL
*"     REFERENCE(I_DATA_VENC_FINAL) TYPE  DZFBDT OPTIONAL
*"     REFERENCE(I_NAO_RAZAO_ESPECIAL) TYPE  CHAR01 DEFAULT ' '
*"     REFERENCE(I_BUDAT) TYPE  FKK_RT_BUDAT OPTIONAL
*"     REFERENCE(I_BLDAT) TYPE  FKK_RT_BLDAT OPTIONAL
*"  TABLES
*"      IT_BSXS STRUCTURE  BSIS OPTIONAL
*"      IT_BSXK STRUCTURE  BSIK OPTIONAL
*"      IT_BSXD STRUCTURE  BSID OPTIONAL
*"      IT_BKPF STRUCTURE  BKPF OPTIONAL
*"      IT_BSEG STRUCTURE  BSEG OPTIONAL
*"----------------------------------------------------------------------

  RANGES: rdata         FOR bsis-zfbdt,
          pwaers        FOR bsis-waers,
          pparid        FOR zde_mov_fornecedor-parid,
          pumskz        FOR bsik-umskz,
* Inicio - falheiros - 23.12.2022
          rdata_bldat   FOR bsid-bldat.

  DATA: v_bldat TYPE char1.
* Fim - falheiros - 23.12.2022

  DATA: it_bsis1 TYPE TABLE OF bsis WITH HEADER LINE,
        it_bsis2 TYPE TABLE OF bsis WITH HEADER LINE,
        wa_bseg  TYPE bseg.

  FIELD-SYMBOLS: <fs_bsxk> TYPE bsik,
                 <fs_bsxd> TYPE bsid.

  CLEAR: it_bsxs[],
         it_bsxk[],
         it_bsxd[].

* Inicio - falheiros - 23.12.2022
  IMPORT v_bldat  TO v_bldat
         FROM MEMORY ID 'M_BLDAT'.
* Fim - falheiros - 23.12.2022

  IF i_nao_razao_especial IS NOT INITIAL.
    pumskz-sign   = 'I'.
    pumskz-option = 'EQ'.
    pumskz-low    = space.
    pumskz-high   = space.
    APPEND pumskz.
  ENDIF.

  IF i_data_venc_ini IS NOT INITIAL AND i_data_venc_final IS INITIAL.
    rdata-sign = 'I'.
    rdata-option = 'GE'.
    rdata-low    = i_data_venc_ini.
    rdata-high   = i_data_venc_ini.
    APPEND rdata.

* Inicio - falheiros - 23.12.2022
    IF v_bldat IS NOT INITIAL.
      APPEND rdata TO rdata_bldat.
      CLEAR: rdata[], rdata.
    ENDIF.
* Fim - falheiros - 23.12.2022

  ELSEIF i_data_venc_ini IS NOT INITIAL AND i_data_venc_final IS NOT INITIAL.
    rdata-sign = 'I'.
    rdata-option = 'BT'.
    rdata-low    = i_data_venc_ini.
    rdata-high   = i_data_venc_final.
    APPEND rdata.

* Inicio - falheiros - 23.12.2022
    IF v_bldat IS NOT INITIAL.
      APPEND rdata TO rdata_bldat.
      CLEAR: rdata[], rdata.
    ENDIF.
* Fim - falheiros - 23.12.2022
  ELSEIF i_data_venc_ini IS INITIAL AND i_data_venc_final IS NOT INITIAL.
    rdata-sign = 'I'.
    rdata-option = 'LE'.
    rdata-low    = i_data_venc_final.
    rdata-high   = i_data_venc_final.
    APPEND rdata.

* Inicio - falheiros - 23.12.2022
    IF v_bldat IS NOT INITIAL.
      APPEND rdata TO rdata_bldat.
      CLEAR: rdata[], rdata.
    ENDIF.
* Fim - falheiros - 23.12.2022
  ENDIF.

* Inicio - falheiros - 23.12.2022
  FREE MEMORY ID 'M_BLDAT'.
* Fim - falheiros - 23.12.2022

  IF i_moeda_doc IS NOT INITIAL.
    pwaers-sign   = 'I'.
    pwaers-option = 'EQ'.
    pwaers-low    = i_moeda_doc.
    pwaers-high   = i_moeda_doc.
    APPEND pwaers.
  ENDIF.

  IF i_parid IS NOT INITIAL.
    pparid-sign   = 'I'.
    pparid-option = 'EQ'.
    pparid-low    = i_parid.
    pparid-high   = i_parid.
    APPEND pparid.
  ENDIF.

* Inicio - falheiros - 23.12.2022
  IF rdata_bldat[] IS NOT INITIAL.
    IF i_forne IS NOT INITIAL.
      SELECT *
        FROM bsis AS b
        INTO TABLE it_bsis1
       WHERE bukrs EQ i_company
         AND zfbdt IN rdata
         AND waers IN pwaers
         AND budat IN i_budat
         AND bldat IN rdata_bldat
         AND EXISTS ( SELECT * FROM bsik AS k
                       WHERE k~bukrs EQ b~bukrs
                         AND k~gjahr EQ b~gjahr
                         AND k~belnr EQ b~belnr
                         AND k~buzei EQ b~buzei
                         AND k~lifnr IN pparid
                         AND k~umskz IN pumskz
* Inicio - falheiros - 23.12.2022
                         AND k~bldat IN rdata_bldat
* Fim - falheiros - 23.12.2022
        ).

      IF it_bsis1[] IS NOT INITIAL.
        SELECT * INTO TABLE it_bsxk
          FROM bsik
           FOR ALL ENTRIES IN it_bsis1
         WHERE bukrs EQ it_bsis1-bukrs
           AND gjahr EQ it_bsis1-gjahr
           AND belnr EQ it_bsis1-belnr
           AND buzei EQ it_bsis1-buzei.

        LOOP AT it_bsis1.
          APPEND it_bsis1 TO it_bsxs.
        ENDLOOP.
      ENDIF.

    ENDIF.

    IF i_cliente IS NOT INITIAL.
      SELECT *
        FROM bsis AS b
        INTO TABLE it_bsis2
       WHERE bukrs EQ i_company
         AND zfbdt IN rdata
         AND waers IN pwaers
         AND budat IN i_budat
         AND bldat IN rdata_bldat
         AND EXISTS ( SELECT * FROM bkpf AS c
                          WHERE c~bukrs  EQ b~bukrs
                          AND   c~belnr  EQ b~belnr
                          AND   c~gjahr  EQ b~gjahr
                          AND   c~bstat  NE 'S' ) " partidas Memo
         AND EXISTS ( SELECT * FROM bsid AS k
                       WHERE k~bukrs EQ b~bukrs
                         AND k~gjahr EQ b~gjahr
                         AND k~belnr EQ b~belnr
                         AND k~buzei EQ b~buzei
                         AND k~umskz IN pumskz
                         AND k~kunnr IN pparid
* Inicio - falheiros - 23.12.2022
                         AND k~bldat IN rdata_bldat
* Fim - falheiros - 23.12.2022
        ).

      IF it_bsis2[] IS NOT INITIAL.

        SELECT * INTO TABLE it_bsxd
          FROM bsid
           FOR ALL ENTRIES IN it_bsis2
         WHERE bukrs EQ it_bsis2-bukrs
           AND gjahr EQ it_bsis2-gjahr
           AND belnr EQ it_bsis2-belnr
           AND buzei EQ it_bsis2-buzei.

        LOOP AT it_bsis2.
          APPEND it_bsis2 TO it_bsxs.
        ENDLOOP.

      ENDIF.

    ENDIF.

  ELSE.

    IF i_forne IS NOT INITIAL.
      SELECT *
        FROM bsis AS b
        INTO TABLE it_bsis1
       WHERE bukrs EQ i_company
         AND zfbdt IN rdata
         AND waers IN pwaers
         AND budat IN i_budat
         AND bldat IN i_bldat
         AND EXISTS ( SELECT * FROM bsik AS k
                       WHERE k~bukrs EQ b~bukrs
                         AND k~gjahr EQ b~gjahr
                         AND k~belnr EQ b~belnr
                         AND k~buzei EQ b~buzei
                         AND k~lifnr IN pparid
                         AND k~umskz IN pumskz
* Inicio - falheiros - 23.12.2022
                         AND k~bldat IN rdata
* Fim - falheiros - 23.12.2022
        ).

      IF it_bsis1[] IS NOT INITIAL.
        SELECT * INTO TABLE it_bsxk
          FROM bsik
           FOR ALL ENTRIES IN it_bsis1
         WHERE bukrs EQ it_bsis1-bukrs
           AND gjahr EQ it_bsis1-gjahr
           AND belnr EQ it_bsis1-belnr
           AND buzei EQ it_bsis1-buzei.

        LOOP AT it_bsis1.
          APPEND it_bsis1 TO it_bsxs.
        ENDLOOP.
      ENDIF.

    ENDIF.

    IF i_cliente IS NOT INITIAL.
      SELECT *
        FROM bsis AS b
        INTO TABLE it_bsis2
       WHERE bukrs EQ i_company
         AND zfbdt IN rdata
         AND waers IN pwaers
         AND budat IN i_budat
         AND bldat IN i_bldat
         AND EXISTS ( SELECT * FROM bkpf AS c
                          WHERE c~bukrs  EQ b~bukrs
                          AND   c~belnr  EQ b~belnr
                          AND   c~gjahr  EQ b~gjahr
                          AND   c~bstat  NE 'S' ) " partidas Memo
         AND EXISTS ( SELECT * FROM bsid AS k
                       WHERE k~bukrs EQ b~bukrs
                         AND k~gjahr EQ b~gjahr
                         AND k~belnr EQ b~belnr
                         AND k~buzei EQ b~buzei
                         AND k~umskz IN pumskz
                         AND k~kunnr IN pparid
* Inicio - falheiros - 23.12.2022
                         AND k~bldat IN rdata
* Fim - falheiros - 23.12.2022
        ).

      IF it_bsis2[] IS NOT INITIAL.

        SELECT * INTO TABLE it_bsxd
          FROM bsid
           FOR ALL ENTRIES IN it_bsis2
         WHERE bukrs EQ it_bsis2-bukrs
           AND gjahr EQ it_bsis2-gjahr
           AND belnr EQ it_bsis2-belnr
           AND buzei EQ it_bsis2-buzei.

        LOOP AT it_bsis2.
          APPEND it_bsis2 TO it_bsxs.
        ENDLOOP.

      ENDIF.

    ENDIF.
* Inicio - falheiros - 23.12.2022
  ENDIF.
* Fim - falheiros - 23.12.2022

  IF it_bsxs[] IS NOT INITIAL.

    SELECT * INTO TABLE it_bkpf
      FROM bkpf
       FOR ALL ENTRIES IN it_bsxs
     WHERE bukrs EQ it_bsxs-bukrs
       AND belnr EQ it_bsxs-belnr
       AND gjahr EQ it_bsxs-gjahr.

    CALL FUNCTION 'FAGL_GET_BSEG_FOR_ALL_ENTRIES'
      EXPORTING
* ---> S4 Migration - 25/07/2023 - LA
        it_for_all_entries = it_bkpf[]
        i_where_clause     = |BUKRS EQ IT_FOR_ALL_ENTRIES-BUKRS AND BELNR EQ IT_FOR_ALL_ENTRIES-BELNR AND GJAHR EQ IT_FOR_ALL_ENTRIES-GJAHR| "AND BUZEI EQ IT_FOR_ALL_ENTRIES-BUZEI|
* <--- S4 Migration - 25/07/2023 - LA
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

  LOOP AT it_bsxk ASSIGNING <fs_bsxk>.
    IF <fs_bsxk>-kidno IS INITIAL.
      READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = <fs_bsxk>-bukrs
                                               belnr = <fs_bsxk>-belnr
                                               gjahr = <fs_bsxk>-gjahr
                                               buzei = <fs_bsxk>-buzei.
      IF ( sy-subrc IS INITIAL ) AND ( wa_bseg-kidno IS NOT INITIAL ).
        <fs_bsxk>-kidno = wa_bseg-kidno.
      ENDIF.
    ENDIF.
  ENDLOOP.

  LOOP AT it_bsxd ASSIGNING <fs_bsxd>.
    IF <fs_bsxd>-kidno IS INITIAL.
      READ TABLE it_bseg INTO wa_bseg WITH KEY bukrs = <fs_bsxd>-bukrs
                                               belnr = <fs_bsxd>-belnr
                                               gjahr = <fs_bsxd>-gjahr
                                               buzei = <fs_bsxd>-buzei.
      IF ( sy-subrc IS INITIAL ) AND ( wa_bseg-kidno IS NOT INITIAL ).
        <fs_bsxd>-kidno = wa_bseg-kidno.
      ENDIF.
    ENDIF.
  ENDLOOP.

ENDFUNCTION.
