*&---------------------------------------------------------------------*
*&  Include           ZFIY0004_FORM
*&---------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form F_tabla_detalle    using st_retencion.
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM f_tabla_detalle    USING pi_ret TYPE zfiys_retencion_cab.
  TYPES: BEGIN OF ty_bsik,
           bukrs TYPE bukrs,
           augbl TYPE augbl,
           gjahr TYPE gjahr,
           belnr TYPE belnr_d,
           blart TYPE blart,
           umskz TYPE umskz,
*Inicio Alteração - Leandro Valentim - 10.01.23 - 99745
           dmbtr TYPE bsak-dmbtr,
*Fim Alteração - Leandro Valentim - 10.01.23 - 99745
         END OF ty_bsik,

*Inicio Alteração - Leandro Valentim - 10.01.23 - 99745
         BEGIN OF ty_bset,
           belnr TYPE bset-belnr,
           bukrs TYPE bset-bukrs,
           gjahr TYPE bset-gjahr,
           mwskz TYPE bset-mwskz,
           shkzg TYPE bset-shkzg,
           hwste TYPE bset-hwste,
         END OF ty_bset.
*Fim Alteração - Leandro Valentim - 10.01.23 - 99745

  DATA:
    stl_with_item  TYPE with_item,
    tl_with_item   TYPE STANDARD TABLE OF with_item,
    t_t059fb       TYPE STANDARD TABLE OF t059fb,
    t_bsik         TYPE STANDARD TABLE OF ty_bsik,
*Inicio Alteração - Leandro Valentim - 10.01.23 - 99745
    t_bsak_aux     TYPE STANDARD TABLE OF ty_bsik,
    t_bset         TYPE STANDARD TABLE OF ty_bset,
*Fim Alteração - Leandro Valentim - 10.01.23 - 99745
    st_bsik        TYPE ty_bsik,
    stl_bseg       TYPE bseg,
    st_t059fb      TYPE t059fb,
    vl_impret      TYPE wt_wt,
    vl_alicuota    TYPE char10,
    vl_accbs       TYPE wt_accbs,
    vl_xqfor       TYPE xqfor.
**** Verificar depois *******
***    vl_certif_afip TYPE zfiyt0030-certif_afip.
********


  DATA: vl_obj_key     TYPE zmmt_ee_zgr-obj_key,
        vl_obj_key_aux TYPE zmmt_ee_zgr-obj_key,
        f_belnr        TYPE rbkp-belnr,
        f_gjahr        TYPE rbkp-gjahr,

        wl_ee_zgr_aux  TYPE zmmt_ee_zgr,
        wl_zgr_docs    TYPE zmmt_ee_zgr_docs,
        wl_zgr         TYPE zmmt_ee_zgr.

  CLEAR: vl_impret.
  SELECT bukrs
         augbl
         gjahr
         belnr
         blart
         umskz
  FROM bsak
  INTO TABLE t_bsik
  WHERE augbl EQ pi_ret-opago
    AND belnr NE pi_ret-opago
    AND bukrs IN br_bukrs
    AND blart NE 'KZ'.

*Inicio Alteração - Leandro Valentim - 10.01.23 - 99745
  SELECT bukrs
         augbl
         gjahr
         belnr
         blart
         umskz
         dmbtr
  FROM bsak
  INTO TABLE t_bsak_aux
  WHERE augbl EQ pi_ret-opago
    AND bukrs IN br_bukrs.

  LOOP AT t_bsak_aux INTO DATA(wl_bsak_aux).
    DATA(vl_tabix) = sy-tabix.
    IF wl_bsak_aux-belnr EQ wl_bsak_aux-augbl.
      DELETE t_bsak_aux INDEX vl_tabix.
    ENDIF.
  ENDLOOP.

  IF t_bsak_aux[] IS NOT INITIAL.
    SELECT belnr bukrs gjahr mwskz shkzg hwste
          FROM bset
          INTO TABLE t_bset
          FOR ALL ENTRIES IN t_bsak_aux
          WHERE bukrs EQ t_bsak_aux-bukrs
            AND belnr EQ t_bsak_aux-belnr
            AND gjahr EQ t_bsak_aux-gjahr.
  ENDIF.
  SORT: t_bsak_aux BY belnr bukrs gjahr,
        t_bset     BY belnr bukrs gjahr.


*---------------------------------------------------------
*Fim Alteração - Leandro Valentim - 10.01.23 - 99745

  LOOP AT t_bseg INTO stl_bseg
    WHERE rebzg NE space
    AND   koart EQ 'K'
    AND   bschl EQ '25'.
    MOVE: stl_bseg-bukrs TO st_bsik-bukrs,
          stl_bseg-belnr TO st_bsik-augbl,
          stl_bseg-umskz TO st_bsik-umskz,
          stl_bseg-rebzg TO st_bsik-belnr,
          stl_bseg-rebzj TO st_bsik-gjahr.

    APPEND st_bsik TO t_bsik.
  ENDLOOP.
  CLEAR stl_bseg.
  SORT t_bsik BY belnr.

  DELETE ADJACENT DUPLICATES FROM t_bsik.

*Agrego los pagos parciales que no fueron encontrados antes.
  LOOP AT t_bsik INTO st_bsik.
    READ TABLE xbkpf
      WITH KEY  belnr = st_bsik-belnr
                bukrs = st_bsik-bukrs.
    IF sy-subrc NE 0.
      SELECT SINGLE *
      FROM bkpf
      INTO CORRESPONDING FIELDS OF xbkpf
      WHERE belnr = st_bsik-belnr
      AND   bukrs = st_bsik-bukrs.
      IF sy-subrc EQ 0.
        APPEND xbkpf TO xbkpf.
      ENDIF.
    ENDIF.
  ENDLOOP.

  IF t_bsik IS NOT INITIAL.

    LOOP AT t_bsik INTO st_bsik.
      LOOP AT xbkpf WHERE belnr  EQ st_bsik-belnr.
*                      AND augbl  EQ st_bsik-augbl.
        REFRESH tl_with_item.
        SELECT  * FROM with_item
                   INTO TABLE tl_with_item
                  WHERE bukrs     = xbkpf-bukrs
                    AND belnr     = st_bsik-belnr "augbl
                    AND gjahr     = xbkpf-gjahr
                    AND augbl     = st_bsik-augbl
                    AND wt_withcd = pi_ret-wt_withcd
                    AND witht     = pi_ret-witht.

*           Si no encontró retenciones en la base de datos usamos
*           la retenciones que están guaradas en una tabla interna
        IF  sy-subrc NE 0.
          REFRESH tl_with_item.
          CLEAR stl_with_item.
          LOOP AT xret
            WHERE witht = pi_ret-witht
              AND belnr = st_bsik-belnr
              AND bukrs = st_bsik-bukrs.

            MOVE-CORRESPONDING xret TO stl_with_item.

            APPEND stl_with_item TO tl_with_item.
          ENDLOOP.
        ENDIF.

*Inicio Alteração - Leandro Valentim - 10.01.23 99745

        READ TABLE t_bsak_aux INTO wl_bsak_aux WITH KEY belnr = st_bsik-belnr
                                                        bukrs = st_bsik-bukrs
                                                        gjahr = st_bsik-gjahr BINARY SEARCH.

        IF sy-subrc EQ 0.
          st_ret_pos-dmbtr = wl_bsak_aux-dmbtr."Monto comprob

          LOOP AT t_bset INTO DATA(wl_bset) WHERE belnr EQ wl_bsak_aux-belnr
                                              AND bukrs EQ wl_bsak_aux-bukrs
                                              AND gjahr EQ wl_bsak_aux-gjahr.


            IF wl_bset-shkzg EQ 'H'.
              wl_bset-hwste = wl_bset-hwste * -1.
            ENDIF.

            IF wl_bset-mwskz EQ 'C1' OR wl_bset-mwskz EQ 'C2' OR wl_bset-mwskz EQ 'C3'.
              st_ret_pos-hwste_iva = wl_bset-hwste."Importe IVA
            ELSEIF wl_bset-mwskz NE 'C1' AND wl_bset-mwskz NE 'C2' AND wl_bset-mwskz NE 'C3'.
              st_ret_pos-hwste_out = wl_bset-hwste."Outos conc
            ENDIF.
          ENDLOOP.
        ENDIF.

        f_belnr         = xbkpf-awkey(10).
        f_gjahr         = xbkpf-awkey+10(4).
        vl_obj_key_aux  = xbkpf-awkey.

        SELECT SINGLE *
          FROM zmmt_ee_zgr_docs INTO wl_zgr_docs
         WHERE bukrs    = xbkpf-bukrs
           AND ft_belnr = f_belnr
           AND ft_gjahr = f_gjahr.

        IF sy-subrc = 0.
          vl_obj_key = wl_zgr_docs-obj_key.
        ELSEIF vl_obj_key_aux IS NOT INITIAL.
          SELECT SINGLE *
            FROM zmmt_ee_zgr INTO wl_ee_zgr_aux
           WHERE obj_key EQ vl_obj_key_aux.

          IF ( sy-subrc = 0 ) AND ( wl_ee_zgr_aux-interface_miro = '54' ).
            vl_obj_key = wl_ee_zgr_aux-obj_key.
          ENDIF.
        ENDIF.

        IF vl_obj_key IS NOT INITIAL.
          SELECT SINGLE *
            FROM zmmt_ee_zgr INTO wl_zgr
           WHERE obj_key = vl_obj_key.
          IF sy-subrc = 0.
            st_ret_pos-oftp_text = wl_zgr-text2.
          ELSE.
            SELECT SINGLE ltext INTO st_ret_pos-oftp_text FROM t003t
                             WHERE spras = sy-langu AND
                                   blart = xbkpf-blart.
          ENDIF.
        ELSE.
          SELECT SINGLE ltext INTO st_ret_pos-oftp_text FROM t003t
                           WHERE spras = sy-langu AND
                                 blart = xbkpf-blart.
        ENDIF.
*Fim Alteração - Leandro Valentim - 10.01.23 - 99745

*       Por cada retencio encontrada la paso a la tabla del formulario
        LOOP AT tl_with_item INTO stl_with_item ."where wt_qsshh > 0.
          IF stl_with_item-wt_accbs1 < 0.
            stl_with_item-wt_accbs1 = stl_with_item-wt_accbs1 * ( - 1 ).
          ENDIF.

          IF stl_with_item-wt_qsshh < 0.
            stl_with_item-wt_qsshh  = stl_with_item-wt_qsshh  * ( - 1 ).
          ENDIF.

          SELECT SINGLE xqfor
          FROM t059z
          INTO vl_xqfor
          WHERE land1     EQ t001-land1
          AND   witht     EQ pi_ret-witht
          AND   wt_withcd EQ pi_ret-wt_withcd.


          IF  vl_xqfor = 'X'.
            vl_accbs = stl_with_item-wt_qsshh + stl_with_item-wt_accbs1.
*     Sacar alicuota para una base con varias alicuotas. y escalas.
            SELECT *
            FROM t059fb
            INTO TABLE t_t059fb
            WHERE land1     EQ t001-land1
            AND   witht     EQ pi_ret-witht
            AND   wt_withcd EQ pi_ret-wt_withcd
            AND   waers     EQ pi_ret-waers
            AND   wt_bbasb  >= vl_accbs
            AND   qmbar     <= vl_accbs.

            IF sy-subrc EQ 0 .
              LOOP AT t_t059fb INTO st_t059fb.
                IF sy-datum => st_t059fb-wt_valid.
                  MOVE st_t059fb-qsatz TO pi_ret-alicuota.

                ENDIF.
              ENDLOOP.
            ENDIF.
          ENDIF.
          IF stl_with_item-wt_qbshh < 0.
*          stl_with_item-wt_qsshb = stl_with_item-wt_qsshb * ( - 1 ).
            stl_with_item-wt_qbshh = stl_with_item-wt_qbshh * ( - 1 ).
          ENDIF.

          IF stl_with_item-wt_accbs < 0.
            stl_with_item-wt_accbs  = stl_with_item-wt_accbs * ( - 1 ).
          ENDIF.

          IF stl_with_item-wt_qsshh < 0.
*          stl_with_item-wt_qbshb = stl_with_item-wt_qbshb * ( - 1 ).
            stl_with_item-wt_qsshh = stl_with_item-wt_qsshh * ( - 1 ).

          ENDIF.

          MOVE:
                xbkpf-bldat              TO st_ret_pos-fecha      ,
                xbkpf-xblnr              TO st_ret_pos-comprobante,
                pi_ret-text2             TO st_ret_pos-text1      ,
                pi_ret-text3             TO st_ret_pos-text2      ,
                pi_ret-cod_fip           TO st_ret_pos-cod_fip    ,
                pi_ret-alicuota          TO st_ret_pos-alicuota   ,
                pi_ret-waers             TO st_ret_pos-waers      ,
                stl_with_item-wt_qsshh   TO st_ret_pos-imp_base   ,
                stl_with_item-wt_qbshh   TO st_ret_pos-retencion  ,
                stl_with_item-wt_accbs1  TO st_ret_pos-base_acum  ,
                stl_with_item-witht      TO st_ret_pos-witht      ,
                stl_with_item-wt_withcd  TO st_ret_pos-wt_withcd  ,
                pi_ret-mni               TO st_ret_pos-mni        .

************** Verificar depois **************
***          CLEAR: vl_certif_afip.
***          SELECT SINGLE certif_afip
***            FROM zfiyt0030
***            INTO vl_certif_afip
***            WHERE bukrs     EQ '0100'
***            AND   xblnr     EQ st_ret_pos-comprobante
***            AND   witht     EQ st_ret_pos-witht
***            AND   wt_withcd EQ st_ret_pos-wt_withcd.
***
***          st_ret_pos-certi_afip   = vl_certif_afip.

*************************

          IF st_ret_pos-base_acum < 0.
            st_ret_pos-base_acum = st_ret_pos-base_acum * ( - 1 ).
          ENDIF.
          IF st_ret_pos-retencion > 0.
            vl_impret = vl_impret + st_ret_pos-retencion .
            APPEND st_ret_pos TO t_ret_pos.
          ELSE.
*              Si no encuentro el valor de la retencion en la tabla WITH_ITEM
*              lo busco de una tabla interna del reporte
            READ TABLE xret
            WITH KEY witht = pi_ret-witht
                     belnr = st_bsik-belnr
                     bukrs = st_bsik-bukrs.
            IF sy-subrc EQ 0.
              IF xret-wt_qbshb < 0.
                xret-wt_qbshb = xret-wt_qbshb * ( - 1 ).
              ENDIF.
              MOVE xret-wt_qbshb TO st_ret_pos-retencion.
              vl_impret = vl_impret + st_ret_pos-retencion .
              APPEND st_ret_pos TO t_ret_pos.
            ENDIF.
          ENDIF.
          CLEAR: st_ret_pos,
                 stl_with_item.
        ENDLOOP.
      ENDLOOP.
    ENDLOOP.
    CLEAR st_retencion-retencion .
    st_retencion-retencion = vl_impret.

  ELSE.
* Si no encontro nada en la bsak y bsik

    DATA: stl_codtab TYPE ty_codtab.
    LOOP AT itab_codtab INTO stl_codtab
      WHERE witht      EQ st_retencion-witht
       AND  wt_withcd  EQ st_retencion-wt_withcd
      AND   augbl      EQ st_retencion-opago.
      MOVE:
      stl_codtab-ctnumber TO stl_with_item-ctnumber,
      stl_codtab-witht    TO stl_with_item-witht,
      stl_codtab-wt_withcd TO stl_with_item-wt_withcd,
      stl_codtab-accbs    TO stl_with_item-wt_accbs,
      stl_codtab-accwt    TO stl_with_item-wt_accwt,
      stl_codtab-qbshb    TO stl_with_item-wt_qbshb,
      stl_codtab-qbshh    TO stl_with_item-wt_qbshh,
      stl_codtab-qsshb    TO stl_with_item-wt_qsshb,
      stl_codtab-qsshh    TO stl_with_item-wt_qsshh,
      stl_codtab-augbl    TO stl_with_item-augbl.


      IF stl_with_item-wt_accbs1 < 0.
        stl_with_item-wt_accbs1 = stl_with_item-wt_accbs1 * ( - 1 ).
      ENDIF.

      IF stl_with_item-wt_qsshh < 0.
        stl_with_item-wt_qsshh  = stl_with_item-wt_qsshh  * ( - 1 ).
      ENDIF.

      SELECT SINGLE xqfor
      FROM t059z
      INTO vl_xqfor
      WHERE land1     EQ t001-land1
      AND   witht     EQ pi_ret-witht
      AND   wt_withcd EQ pi_ret-wt_withcd.


      IF  vl_xqfor = 'X'.
        vl_accbs = stl_with_item-wt_qsshh + stl_with_item-wt_accbs1.
*     Sacar alicuota para una base con varias alicuotas. y escalas.
        SELECT *
        FROM t059fb
        INTO TABLE t_t059fb
        WHERE land1     EQ t001-land1
        AND   witht     EQ pi_ret-witht
        AND   wt_withcd EQ pi_ret-wt_withcd
        AND   waers     EQ pi_ret-waers
        AND   wt_bbasb  >= vl_accbs
        AND   qmbar     <= vl_accbs.

        IF sy-subrc EQ 0 .
          LOOP AT t_t059fb INTO st_t059fb.
            IF sy-datum => st_t059fb-wt_valid.

              MOVE st_t059fb-qsatz TO pi_ret-alicuota.
* Eliminado en 17.11.2011 - Sonda
*             pi_ret-alicuota = pi_ret-alicuota * 100.
* Fin 17.11.2011
*            ENDIF.
            ENDIF.
          ENDLOOP.
        ENDIF.
      ENDIF.
      IF stl_with_item-wt_qsshb < 0.
        stl_with_item-wt_qsshb = stl_with_item-wt_qsshb * ( - 1 ).
      ENDIF.

      IF stl_with_item-wt_accbs < 0.
        stl_with_item-wt_accbs  = stl_with_item-wt_accbs * ( - 1 ).
      ENDIF.

      IF stl_with_item-wt_qbshb < 0.
        stl_with_item-wt_qbshb = stl_with_item-wt_qbshb * ( - 1 ).
      ENDIF.


      MOVE:
                  xbkpf-bldat              TO st_ret_pos-fecha      ,
                  xbkpf-xblnr              TO st_ret_pos-comprobante,
                  pi_ret-text2             TO st_ret_pos-text1      ,
                  pi_ret-text3             TO st_ret_pos-text2      ,
                  pi_ret-cod_fip           TO st_ret_pos-cod_fip    ,
                  pi_ret-alicuota          TO st_ret_pos-alicuota   ,
                  pi_ret-waers             TO st_ret_pos-waers      ,
                  stl_with_item-wt_qsshb   TO st_ret_pos-imp_base   ,
                  stl_with_item-wt_qbshb   TO st_ret_pos-retencion  ,
*          stl_with_item-wt_accbs   TO st_ret_pos-base_acum  ,
                  stl_with_item-wt_accbs1  TO st_ret_pos-base_acum  ,
                  stl_with_item-witht      TO st_ret_pos-witht      ,
                  stl_with_item-wt_withcd  TO st_ret_pos-wt_withcd  ,
*                  st_ret_pos-retencion     TO st_ret_pos-imp_tot_ret,
                  pi_ret-mni               TO st_ret_pos-mni        .

***************** Verificar depois  ****************
***      CLEAR: vl_certif_afip.
***      SELECT SINGLE certif_afip
***        FROM zfiyt0030
***        INTO vl_certif_afip
***        WHERE bukrs     EQ '0100'
***        AND   xblnr     EQ st_ret_pos-comprobante
***        AND   witht     EQ st_ret_pos-witht
***        AND   wt_withcd EQ st_ret_pos-wt_withcd.
***
***      st_ret_pos-certi_afip   = vl_certif_afip.
**********************************************

      IF st_ret_pos-base_acum < 0.
        st_ret_pos-base_acum = st_ret_pos-base_acum * ( - 1 ).
      ENDIF.
      IF st_ret_pos-retencion > 0.
        APPEND st_ret_pos TO t_ret_pos.
        vl_impret = vl_impret + st_ret_pos-retencion .
      ENDIF.
      CLEAR: st_ret_pos,
             stl_with_item.
    ENDLOOP.

    CLEAR st_retencion-retencion .
    st_retencion-retencion = vl_impret.


  ENDIF.
ENDFORM.                    "F_tabla_detalle
*&---------------------------------------------------------------------*
*&      Form  OPEN_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM open_sapscript.

*  DATA : xtdform LIKE t059p-tdform.    " Note  441516
*  CLEAR  xtdform.                      " Note  441516
*
*  itcpo-tdimmed    = '*'.                        " kz sofort drucken
*  itcpo-tddelete   = '*'.                        " kz freigbe nach Druck
*  itcpo-tdlifetime = '7'.                        " verfalltage
*  itcpo-tdpreview  = 'X'.                        " druckansicht
*
*  SELECT tdform FROM t059p INTO  xtdform              " Note  441516
*                           WHERE land1 = 'AR'.        " Note  441516
*    IF NOT xtdform IS INITIAL.         " Note  441516
*      EXIT.                            " Note  441516
*    ENDIF.                             " Note  441516
*  ENDSELECT.                           " Note  441516
*
*  IF xtdform IS INITIAL.               " Note  441516
*    xtdform = 'F_RFWTCT10_10'.         " Note  441516
*  ENDIF.                               " Note  441516
*
*  CALL FUNCTION 'OPEN_FORM'                      " open form for output
**        exporting form    = 'J_1A_F015_WTHCTF'
**        exporting form    = 'F_RFWTCT10_10'            Note  441516
*         EXPORTING form    = xtdform   " Note  441516
*                 dialog  = 'X'
*                 OPTIONS = itcpo
*                 device  = 'PRINTER'.

ENDFORM.                    " OPEN_SAPSCRIPT
*&---------------------------------------------------------------------*
*&      Form  READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_company_data.
  CLEAR: t001, t001z, xt001. ", sadr.

  SELECT SINGLE * FROM t001 WHERE bukrs = bkpf-bukrs.

* -------------------------- Get Address ------------------------------
  CLEAR xaddr1_sel.
  xaddr1_sel-addrnumber = t001-adrnr.                       "SADR40A
  xaddr1_sel-addrhandle = ' '.
  xaddr1_sel-nation = ' '.
* xaddr1_sel-date = bkpf-budat.
  CALL FUNCTION 'ADDR_GET'                                  "SADR40A
    EXPORTING
      address_selection = xaddr1_sel
      read_sadr_only    = ' '
    IMPORTING
      sadr              = xsadr
    EXCEPTIONS
      parameter_error   = 1
      address_not_exist = 2
      version_not_exist = 3
      internal_error    = 4.

*  Old access to address !
*  select single * from sadr where adrnr eq t001-adrnr
*                            and   natio eq space.

  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1ATID'.

  xt001-stcdt = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AIDN'.

  xt001-stcd1 = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AGIN'.

  xstcd2 = t001z-paval.
  CALL FUNCTION 'BF_PUT_DASHES_TO_STCD2'
    EXPORTING
      i_stcd2 = xstcd2
    IMPORTING
      e_stcd2 = xt001-stcd2.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AGIB'.

  xt001-stcd3 = t001z-paval.

  CLEAR t001z.
  SELECT SINGLE * FROM t001z WHERE bukrs = t001-bukrs
                             AND   party = 'J1AFTV'.

  xt001-fityp = t001z-paval.

  PERFORM read_j1afitpt USING 'D' xt001-fityp.

  xt001-fitptxt = xfitpt-text30.

* --> Begin of OSS                                          Note 393173
* -->  Select all withholding tax types that permit Alrdy W.held amount
  SELECT land1 witht FROM t059p
     INTO CORRESPONDING FIELDS OF TABLE x_t059p
     WHERE land1 EQ xsadr-land1
       AND wt_alwt EQ 'X'  ORDER BY PRIMARY KEY .
* <-- End   of OSS                                          Note 393173

ENDFORM.                    " READ_COMPANY_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_CURRENCY_TEXTS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_currency_texts.
  CHECK tcurt-waers NE t001-waers.
  CLEAR tcurt.

  SELECT SINGLE * FROM tcurt WHERE spras = 'S'
                             AND   waers = t001-waers.
ENDFORM.                    " READ_CURRENCY_TEXTS
*&---------------------------------------------------------------------*
*&      Form  READ_POSTING_KEY
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_posting_key USING f_bschl.
  CLEAR: tbsl, xtbsl.

  READ TABLE xtbsl WITH KEY f_bschl.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM tbsl WHERE bschl = f_bschl.

  MOVE-CORRESPONDING tbsl TO xtbsl.

  APPEND xtbsl.
ENDFORM.                    " READ_POSTING_KEY
*&---------------------------------------------------------------------*
*&      Form  READ_CPD_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_cpd_data USING f_belnr f_buzei f_gjahr.
  CLEAR xbsec.
  SELECT SINGLE * FROM bsec WHERE bukrs = bkpf-bukrs
                            AND   belnr = f_belnr
                            AND   gjahr = f_gjahr
                            AND   buzei = f_buzei.

  MOVE-CORRESPONDING bsec TO xbsec.

  IF     bsec-stcd1 IS INITIAL AND
     NOT bsec-land1 IS INITIAL AND
     NOT bsec-stkzn IS INITIAL.
    SELECT SINGLE * FROM j_1afrid
                    WHERE land1 = bsec-land1
                    AND   stkzn = bsec-stkzn.
    xbsec-stcd1 = j_1afrid-j_1afpid.
*   xbsec-stcd1 = bsec-j_1acfop.
  ENDIF.

  IF NOT bsec-fityp IS INITIAL.
    PERFORM read_j1afitpt USING 'K' bsec-fityp.
    xbsec-fitptxt = xfitpt-text30.
  ENDIF.

  CALL FUNCTION 'BF_PUT_DASHES_TO_STCD2'
    EXPORTING
      i_stcd2 = bsec-stcd2
    IMPORTING
      e_stcd2 = xbsec-stcd2.

  xbsec-lifnr = bseg-lifnr.
  APPEND xbsec.
ENDFORM.                    " READ_CPD_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_VENDOR_DATA
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_vendor_data.
  IF xxcpdd NE space.
    CLEAR xbsec.
    READ TABLE xbsec WITH KEY empfg = codtab-empfg.

    MOVE-CORRESPONDING xbsec TO xlfa1.
    CHECK 1 = 2.
  ENDIF.

  CHECK xlfa1-lifnr NE xlifnr.

  CLEAR: xlfa1, lfa1.

  READ TABLE xlfa1 WITH KEY xlifnr.
  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM lfa1 WHERE lifnr = xlifnr.

  IF NOT lfa1-fiskn IS INITIAL.
    SELECT SINGLE * FROM lfa1 WHERE lifnr = lfa1-fiskn.
  ENDIF.

  MOVE-CORRESPONDING lfa1 TO xlfa1.

*  if not lfa1-j_1acfop is initial.
*     xlfa1-stcd1 = lfa1-j_1acfop.
*  endif.

  IF NOT lfa1-land1 IS INITIAL AND
     lfa1-land1 <> 'AR'        AND                      " Note 339702
     NOT lfa1-stkzn IS INITIAL.
    SELECT SINGLE * FROM j_1afrid
                    WHERE land1 = lfa1-land1
                    AND   stkzn = lfa1-stkzn.
    xlfa1-stcd1 = j_1afrid-j_1afpid.
  ENDIF.

  IF NOT lfa1-fityp IS INITIAL.
    PERFORM read_j1afitpt USING 'K' lfa1-fityp.
    xlfa1-fitptxt = xfitpt-text30.
  ENDIF.

  xstcd2 = xlfa1-stcd2.
  CALL FUNCTION 'BF_PUT_DASHES_TO_STCD2'
    EXPORTING
      i_stcd2 = xstcd2
    IMPORTING
      e_stcd2 = xlfa1-stcd2.

  xlfa1-lifnr = xlifnr.
  APPEND xlfa1.
ENDFORM.                    " READ_VENDOR_DATA
*&---------------------------------------------------------------------*
*&      Form  READ_J1AFITPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_j1afitpt USING f_koart f_fiscal_vat.
  CLEAR: j_1afitpt, xfitpt.

  READ TABLE xfitpt WITH KEY koart = f_koart
                             fityp = f_fiscal_vat.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM j_1afitpt WHERE spras    = 'S'
                                 AND   koart    = f_koart
                                 AND   j_1afitp = f_fiscal_vat.

  CHECK sy-subrc EQ 0.

  MOVE-CORRESPONDING j_1afitpt TO xfitpt.                         " !!!
  APPEND xfitpt.
ENDFORM.                    " READ_J1AFITPT
*&---------------------------------------------------------------------*
*&      Form  CREATE_PAYTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM create_paytab.
  CLEAR: paytab. " , j_1awtpkey.
  CLEAR xret.
  MOVE-CORRESPONDING: bseg TO paytab.
  APPEND  bseg TO t_bseg.
* delete 25/06/2010
*  IF xtbsl-xzahl NE space.
* delete 25/06/2010
* read the withholding data segment
  SELECT * FROM with_item WHERE bukrs      EQ bkpf-bukrs
                          AND   belnr     EQ bseg-belnr
                          AND   gjahr     EQ bseg-gjahr
                          AND   buzei     EQ bseg-buzei
                          AND   witht     IN s_witht
                          AND   wt_withcd NE space
                          AND   wt_stat   EQ space.
*                         and   wt_qbshh  ne 0.

    CHECK with_item-wt_qbshh NE 0 OR
          with_item-wt_qbshb NE 0.


    IF bseg-rebzg IS NOT INITIAL.
      MOVE:
            bseg-rebzg        TO xret-belnr    ,
            bseg-bukrs         TO xret-bukrs   ,
            with_item-witht    TO xret-witht   .
      xret-wt_qsshh = with_item-wt_qsshh.
      xret-wt_qbshh = with_item-wt_qbshh.
      xret-wt_accbs1 = with_item-wt_accbs1.
      xret-wt_withcd = with_item-wt_withcd.
      IF with_item-wt_qbshh IS INITIAL.
        MOVE  with_item-wt_qbshb TO xret-wt_qbshb.
      ELSE.
        MOVE  with_item-wt_qbshh TO xret-wt_qbshb.
      ENDIF.
      APPEND xret.
    ENDIF.


    xlifnr = bseg-lifnr.                   "     NOTE 337731
**Note 1009246 Begins
    wa_xlifnr-lifnr = bseg-lifnr.
    COLLECT wa_xlifnr INTO itab_xlifnr.
**Note 1009246 Ends



    PERFORM read_withhld_code.

    CHECK xt059z-wt_posin = '1'.
*                                       Only WT postings that lead to a
*                                       reduction of the invoice amount
*                                       are reported to the vendor.
*                                       Thus grossing up posting is
*                                       excluded.

*      if bseg-shkzg = 'H'.
*         with_item-wt_qbshh = with_item-wt_qbshh * -1.
*         with_item-wt_qbshb = with_item-wt_qbshb * -1.
*      endif.

    PERFORM read_off_withhld_code.          " Writes t059o to xt059o
    PERFORM fill_pay_codtab USING 'X'.
  ENDSELECT.
* delete 25/06/2010
*  ELSE.
*    CLEAR with_item.
*    PERFORM fill_pay_codtab USING ' '.
*  ENDIF.
* delete 25/06/2010

ENDFORM.                    " CREATE_PAYTAB
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_TYPE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_type.
  CLEAR: xt059p, t059p, t059u.

  READ TABLE xt059p WITH KEY land1 = t001-land1
                             witht = codtab-witht.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t059p WHERE land1 = t001-land1
                             AND   witht = codtab-witht.

  SELECT SINGLE * FROM t059u WHERE spras = 'S'
                             AND   land1 = t001-land1
                             AND   witht = codtab-witht.

  MOVE-CORRESPONDING t059p TO xt059p.
  xt059p-text = t059u-text40.

  APPEND xt059p.
ENDFORM.                    " READ_WITHHLD_TYPE
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_REGION
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_region.
  CHECK xt059p-regio NE space.
  CHECK xt005u-land1 NE t001-land1 OR
        xt005u-bland NE xt059p-regio.

  CLEAR: xt005u, t005u.

  READ TABLE xt005u WITH KEY spras = 'S'
                             land1 = t001-land1
                             bland = xt059p-regio.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t005u WHERE spras = 'S'
                             AND   land1 = t001-land1
                             AND   bland = xt059p-regio.

  MOVE-CORRESPONDING t005u TO xt005u.

  APPEND xt005u.
ENDFORM.                    " READ_WITHHLD_REGION
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHLD_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_withhld_code.
  CLEAR: xt059z, t059z.

  READ TABLE xt059z WITH KEY land1      = t001-land1
                             witht      = with_item-witht
                             wt_withcd  = with_item-wt_withcd.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t059z WHERE land1     = t001-land1
                               AND witht     = with_item-witht
                               AND wt_withcd = with_item-wt_withcd.

  MOVE-CORRESPONDING t059z TO xt059z.

  APPEND xt059z.
ENDFORM.                    " READ_WITHHLD_CODE
*&---------------------------------------------------------------------*
*&      Form  READ_OFF_WITHHLD_CODE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_off_withhld_code.
  CLEAR: t059o, xt059o.

  READ TABLE xt059o WITH KEY land1    = t001-land1
                             wt_qscod = xt059z-qscod.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM t059o WHERE land1 = t001-land1
                             AND   wt_qscod = xt059z-qscod.

  MOVE-CORRESPONDING t059o TO xt059o.

* Reparatur sprachabhängige Textausgabe                      "P45K059421
  SELECT SINGLE * FROM t059ot WHERE spras = 'S'
                                AND land1 = t001-land1
                                AND wt_qscod = xt059z-qscod.
  IF sy-subrc EQ 0.
    xt059o-text40 = t059ot-text40.
  ENDIF.
* Ende                                                       "P45K059421

  APPEND xt059o.
ENDFORM.                    " READ_OFF_WITHHLD_CODE
*&---------------------------------------------------------------------*
*&      Form  FILL_PAY_CODTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_pay_codtab USING f_fill_cod.
  CLEAR: paytab, codtab.

*  if bseg-xcpdd ne space.                                " P45K0390668
  IF bseg-xcpdd NE space AND                              " P45K0390668
     lfa1-xcpdk NE space.                                 " P45K0390668

    PERFORM read_cpd_data USING bkpf-belnr bseg-buzei bkpf-gjahr.
    MOVE-CORRESPONDING: xbsec TO paytab,
                        xbsec TO codtab.
  ENDIF.

  MOVE-CORRESPONDING bseg TO paytab.

  paytab-witht    = with_item-witht.
  paytab-ctnumber = with_item-ctnumber.                 " Corr. 3645505
  paytab-xzahl    = xtbsl-xzahl.

  COLLECT paytab.

  CHECK f_fill_cod NE space.

  codtab-lifnr      = bseg-lifnr.  "Note 1009246
  codtab-witht      = with_item-witht.
  codtab-ctnumber   = with_item-ctnumber.                 " Corr. 3645505
  codtab-qscod      = xt059z-qscod.
  codtab-qsbez      = xt059o-text40.
  codtab-accbs      = with_item-wt_accbs.
  codtab-accwt      = with_item-wt_accwt.
  codtab-qbshb      = with_item-wt_qbshb.
  codtab-qbshh      = with_item-wt_qbshh.
  codtab-qsshb      = with_item-wt_qsshb.
  codtab-qsshh      = with_item-wt_qsshh.

  codtab-wt_withcd  = with_item-wt_withcd.
  .
  APPEND  codtab TO codtab2.
  COLLECT codtab.


  IF with_item-wt_accbs NE 0 OR with_item-wt_accwt NE 0.
* take the accumulated values only once per document, do not add them!
    READ TABLE codtab INDEX sy-tabix.
    IF with_item-wt_accbs LT 0.
      codtab-accbs = with_item-wt_accbs * -1.
    ELSE.
      codtab-accbs = with_item-wt_accbs.
    ENDIF.
    IF with_item-wt_accwt LT 0.
      codtab-accwt = with_item-wt_accwt * -1.
    ELSE.
      codtab-accwt = with_item-wt_accwt.
    ENDIF.
    MODIFY codtab INDEX sy-tabix.
  ENDIF.
ENDFORM.                    " FILL_PAY_CODTAB
*&---------------------------------------------------------------------*
*&      Form  PRINT_CERTIFICATE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_certificate.
  DATA: stl_codtab TYPE ty_codtab.

  CLEAR xlines.

  CLEAR: wa_lifnr,
         itab_lifnr,
         st_retencion.
*         itab_codtab,
*         t_retencion.
*  REFRESH t_retencion.

*-----------Delete 28/06/2010
*  LOOP AT paytab WHERE xzahl NE space.
*    EXIT.
*  ENDLOOP.
*
*  CHECK sy-subrc = 0.
*-----------Delete 28/06/2010
  SORT: codtab, clrdtab.

***Note 1009246 begins
  LOOP AT codtab.
    AT NEW lifnr.
      MOVE codtab-lifnr TO wa_lifnr-lifnr.
      APPEND wa_lifnr TO itab_lifnr.
    ENDAT.
  ENDLOOP.
***Note 1009246 ends

  CLEAR copy_no.

***Note 1009246 begins
*    LOOP AT CODTAB.
*     AT NEW WITHT.

  LOOP AT itab_lifnr INTO wa_lifnr.

*   CLEAR itab_codtab.
    LOOP AT codtab WHERE lifnr = wa_lifnr-lifnr.
      codtab-augbl = bkpf-belnr.
      APPEND codtab TO itab_codtab.
    ENDLOOP.
    SORT itab_codtab BY witht.

    LOOP AT itab_codtab INTO codtab.
      MOVE codtab TO stl_codtab.
*     elimina los fondo de reparo.
      IF codtab-witht EQ 'FR'.
        CONTINUE.
      ENDIF.
      AT NEW wt_withcd."witht.
        xlifnr = codtab-lifnr.
        PERFORM read_vendor_data.
***Note 1009246 ends

        IF xxcpdd NE space.
          PERFORM read_vendor_data.                   " Read CPD data
        ENDIF.
        sy-pagno = 1.

        PERFORM read_withhld_type. " Reads t059p and t059u into xt059p
        PERFORM start_sapscript.
        PERFORM read_withhld_region.          " Reads t005 into xt005u
*       perform read_j1a104.                  " Selects j_1a104
        PERFORM fill_general_fields.          " Fills fiwtform_01
        PERFORM print_clrdtab.

        IF xt059p-wt_accpt = 0.
          CLEAR fiwtform_01-pdata.
        ELSE.
          fiwtform_01-pdata = 'X'.
        ENDIF.

*       CALL FUNCTION 'WRITE_FORM'
*         EXPORTING
*           element = 'HEADER_OWC'.
        PERFORM f_completo_st_retencion USING fiwtform_01 stl_codtab  CHANGING st_retencion.
      ENDAT.

      PERFORM print_codtab.

      AT END OF wt_withcd."witht.
        SUM.
        PERFORM print_totals.
        PERFORM print_final_text.
*       CALL FUNCTION 'END_FORM'.
        PERFORM f_completo_st_retencion USING fiwtform_01 stl_codtab  CHANGING st_retencion.

      ENDAT.
      IF st_retencion IS NOT INITIAL.
        APPEND st_retencion TO t_retencion.
        CLEAR st_retencion.
      ENDIF.

    ENDLOOP.

    CLEAR st_retencion.
  ENDLOOP.        "Note 1009246

*  ADD 1 TO copy_no.

ENDFORM.                    " PRINT_CERTIFICATE

*&---------------------------------------------------------------------*
*&      Form  f_llamo_smartforms
*&---------------------------------------------------------------------*
*FORM F_LLAMO_SMARTFORMS USING PI_RETENCION TYPE ZFIYS_RETENCION_CAB OREF CK_INSTANCE TYPE REF TO ZCL_MEMORY_VARIAVEIS.
*
**--- Internal tables, Structures and Variables used for PDF conversion
*  DATA: IT_OTF                  TYPE STANDARD TABLE OF ITCOO,
*        IT_DOCS                 TYPE STANDARD TABLE OF DOCS,
*        IT_LINES                TYPE STANDARD TABLE OF TLINE,
*        V_LINES                 TYPE TLINE,
*        ST_JOB_OUTPUT_INFO      TYPE SSFCRESCL,
*        ST_DOCUMENT_OUTPUT_INFO TYPE SSFCRESPD,
*        ST_JOB_OUTPUT_OPTIONS   TYPE SSFCRESOP,
*        ST_OUTPUT_OPTIONS       TYPE SSFCOMPOP,
*        ST_CONTROL_PARAMETERS   TYPE SSFCTRLOP,
*        V_LEN_IN                TYPE SO_OBJ_LEN,
*        V_LANGUAGE              TYPE SFLANGU VALUE 'S',
*        V_E_DEVTYPE             TYPE RSPOPTYPE,
*        V_BIN_FILESIZE          TYPE I,
*        V_NAME                  TYPE STRING,
*        V_PATH                  TYPE STRING,
*        V_FULLPATH              TYPE STRING,
*        V_FILTER                TYPE STRING,
*        V_UACT                  TYPE I,
*        V_GUIOBJ                TYPE REF TO CL_GUI_FRONTEND_SERVICES,
*        V_FILENAME              TYPE STRING,
*        V_FILENAME_S            TYPE STRING,
*        V_CERTIFICADO           TYPE ZFIYS_RETENCION_CAB-CERTIFICADO,
*        V_ENDERECO              TYPE ZPARAMETROS-VALOR.
*
*  DATA: HANDLE TYPE REF TO ZCL_MEMORY_VARIAVEIS_AREA,
*        ROOT   TYPE REF TO ZCL_MEMORY_VARIAVEIS.
*
*  DATA: LV_ACTIVE     TYPE TDBOOL,
*        WGC_SMARTFORM TYPE RS38L_FNAM,
*        LS_CONTROL    TYPE SSFCTRLOP,
*        LS_OPTIONS    TYPE SSFCOMPOP.
*
*
*  CONSTANTS: C_PRINTER(7) TYPE C VALUE 'PRINTER'.
*
*  DATA : FORM_NAME    TYPE FPNAME,
*         GV_FNAME(30),
*         W_COPIA      TYPE C. "Flag para saber si es duplicado o original.
*
*  CONSTANTS: CS_FORM TYPE NA_FNAME VALUE 'ZFIY0002',
*             C_X     TYPE C VALUE 'X'.
*
*  CALL FUNCTION 'SSF_STATUS_INFO'
*    EXPORTING
*      I_FORMNAME = CS_FORM
*    IMPORTING
*      O_ACTIVE   = LV_ACTIVE.
*
*  IF LV_ACTIVE IS INITIAL.
*    STOP.
*  ENDIF.
*
*  CALL FUNCTION 'SSF_FUNCTION_MODULE_NAME'
*    EXPORTING
*      FORMNAME           = CS_FORM
*    IMPORTING
*      FM_NAME            = WGC_SMARTFORM
*    EXCEPTIONS
*      NO_FORM            = 1
*      NO_FUNCTION_MODULE = 2
*      OTHERS             = 3.
*  IF SY-SUBRC <> 0.
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*  ENDIF.
**  Impresora
*  LS_CONTROL-NO_DIALOG = 'X'. "Evita la pantalla de opciones de salida del formulario
*  LS_OPTIONS-TDDEST   = P_IMPR.
*  LS_OPTIONS-TDIMMED  = C_X.
*  LS_OPTIONS-TDNEWID  = C_X.
*  LS_OPTIONS-TDNOARCH = C_X.
*  LS_OPTIONS-TDCOPIES = S_COPY.
*
** Imprime o visualiza
**  IF RB_VIEW NE 'X'.
**    CLEAR LS_CONTROL-PREVIEW.
**    LS_CONTROL-DEVICE = C_PRINTER.
**    LS_CONTROL-NO_DIALOG = 'X'.
**    LS_CONTROL-GETOTF = 'X'.
**  ELSE.
**    LS_CONTROL-PREVIEW = 'X'.
**  ENDIF.
*
** Job o On-line
*  IF SY-BATCH IS INITIAL OR SY-TCODE = 'ZFIY0036' OR CK_INSTANCE EQ ABAP_TRUE.
*    IF RB_PRNT EQ 'X' .
*      LS_CONTROL-PREVIEW = SPACE.
*      LS_CONTROL-DEVICE  = C_PRINTER.
*      LS_CONTROL-GETOTF  = 'X'.
*    ELSE.
*      LS_CONTROL-PREVIEW = 'X'.
*      LS_CONTROL-DEVICE  = C_PRINTER.
*      LS_CONTROL-GETOTF  = ' '.
*    ENDIF.
*  ELSE.
*    LS_CONTROL-PREVIEW = SPACE.
*    LS_CONTROL-DEVICE  = C_PRINTER.
*    LS_CONTROL-GETOTF  = 'X'.
*  ENDIF.
*
**  PERFORM f_logo CHANGING st_retencion-form.
**  PERFORM f_completo_st_retencion USING fiwtform_01 pi_codtab  CHANGING st_retencion.
*
**  DO 2 TIMES.
*
**    IF sy-index EQ 1.
*  CLEAR W_COPIA.
**    ELSE.
**      w_copia = 'X'.
**    ENDIF.
*
*
*  CLEAR V_FULLPATH.
*
*  V_CERTIFICADO = PI_RETENCION-CERTIFICADO.
*
*  REPLACE ALL OCCURRENCES OF '-' IN V_CERTIFICADO WITH ''.
*
*  "REPLACE '-' WITH initial INTO V_CERTIFICADO.
*  IF SY-BATCH IS NOT INITIAL.
*    CONCATENATE P_PATH 'Ret_' PI_RETENCION-WT_WITHCD '_' PI_RETENCION-WITHT '_' V_CERTIFICADO '_' PI_RETENCION-OPAGO '.pdf' INTO V_FULLPATH.
*  ELSE.
*    CONCATENATE P_PATH '\Ret_' PI_RETENCION-WT_WITHCD '_' PI_RETENCION-WITHT '_' V_CERTIFICADO '_' PI_RETENCION-OPAGO '.pdf'  INTO V_FULLPATH.
*  ENDIF.
*
*  EXPORT V_FULLPATH TO MEMORY ID 'ZV_FULLPATH'. "(Colocas en memoria)
*
**   Se llama al Smartform
*  CALL FUNCTION WGC_SMARTFORM
*    EXPORTING
*      USER_SETTINGS        = ' '
*      CONTROL_PARAMETERS   = LS_CONTROL
*      OUTPUT_OPTIONS       = LS_OPTIONS
*      GV_CHK_FECHA_IMP     = ''
*      GS_RETENCION         = PI_RETENCION
*      V_FLAG               = W_COPIA
*    IMPORTING
*      DOCUMENT_OUTPUT_INFO = ST_DOCUMENT_OUTPUT_INFO
*      JOB_OUTPUT_INFO      = ST_JOB_OUTPUT_INFO
*      JOB_OUTPUT_OPTIONS   = ST_JOB_OUTPUT_OPTIONS
*    TABLES
*      GT_RETENCION         = T_RET_POS
*    EXCEPTIONS
*      FORMATTING_ERROR     = 1
*      INTERNAL_ERROR       = 2
*      SEND_ERROR           = 3
*      USER_CANCELED        = 4
*      OTHERS               = 5.
*
*  IF SY-SUBRC <> 0.
*
*    MESSAGE ID SY-MSGID TYPE SY-MSGTY
*    NUMBER SY-MSGNO
*      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*  ELSE.
*
*    IF SY-BATCH IS NOT INITIAL OR SY-TCODE EQ 'ZFIY0036' OR CK_INSTANCE EQ ABAP_TRUE.
*
*      DATA:" WA_ZLEST0007    TYPE ZLEST0007,
*        "WA_ZSDYT0053    TYPE ZSDYT0053,
*        BIN_FILESIZE    TYPE I,
*        PDF_TAB         LIKE TLINE OCCURS 0 WITH HEADER LINE,
*        LS_PDF_STRING_X TYPE XSTRING.
*      "LT_PDF          TYPE TABLE OF CHAR80,
*      "LS_PDF          TYPE CHAR80,
*      "FILENAME        TYPE STRING.
*
**      SELECT SINGLE * INTO WA_ZLEST0007
**        FROM ZLEST0007
**       WHERE ID_INTERFACE = '33'
**         AND ID_CTG       = 'PDF'
**         AND PREFIX       = 'OP'.
**
**      IF SY-SUBRC IS INITIAL.
*
**        SELECT SINGLE * INTO WA_ZSDYT0053
**          FROM ZSDYT0053
**         WHERE BUKRS     EQ XBKPF-BUKRS
**           AND GJAHR     EQ XBKPF-GJAHR
**           AND AUGBL     EQ XBKPF-BELNR
**           AND WT_WITHCD EQ PI_RETENCION-WT_WITHCD
**           AND WITHT     EQ PI_RETENCION-WITHT.
**
**        IF SY-SUBRC IS NOT INITIAL.
**          WA_ZSDYT0053-BUKRS       = XBKPF-BUKRS.
**          WA_ZSDYT0053-GJAHR       = XBKPF-GJAHR.
**          WA_ZSDYT0053-AUGBL       = XBKPF-BELNR.
**          WA_ZSDYT0053-CERTIFICADO = V_CERTIFICADO.
**          WA_ZSDYT0053-WT_WITHCD   = PI_RETENCION-WT_WITHCD.
**          WA_ZSDYT0053-WITHT       = PI_RETENCION-WITHT.
**          WA_ZSDYT0053-BELNR_O     = PI_RETENCION-OPAGO.
**          WA_ZSDYT0053-GJAHR_O     = PI_RETENCION-GJAHR.
**        ENDIF.
**
**
**        CONCATENATE 'Ret_' PI_RETENCION-WT_WITHCD '_' PI_RETENCION-WITHT '_' V_CERTIFICADO '_' PI_RETENCION-OPAGO '.pdf' INTO WA_ZSDYT0053-NM_ARQUIVO.
**        CONCATENATE WA_ZLEST0007-PATHUNIX WA_ZSDYT0053-NM_ARQUIVO INTO FILENAME.
*
*      CALL FUNCTION 'CONVERT_OTF'
*        EXPORTING
*          FORMAT        = 'PDF'
*          MAX_LINEWIDTH = 132
*        IMPORTING
*          BIN_FILESIZE  = BIN_FILESIZE
*          BIN_FILE      = LS_PDF_STRING_X
*        TABLES
*          OTF           = ST_JOB_OUTPUT_INFO-OTFDATA[]
*          LINES         = PDF_TAB.
*
*      IF SY-SUBRC IS INITIAL.
*
*        CASE CK_INSTANCE .
*          WHEN ABAP_TRUE.
**            CONCATENATE XBKPF-BUKRS XBKPF-GJAHR XBKPF-BELNR PI_RETENCION-WT_WITHCD PI_RETENCION-WITHT V_CERTIFICADO PI_RETENCION-OPAGO PI_RETENCION-GJAHR INTO DATA(I_NM_TIPO) SEPARATED BY '-'.
**            OREF->ADD_TEXTOS_TIPO( I_NM_TIPO = CONV #( I_NM_TIPO ) I_XSTRING = LS_PDF_STRING_X I_OTF = ST_JOB_OUTPUT_INFO-OTFDATA ).
**            EXIT.
*          WHEN ABAP_FALSE.
*            DATA: ORDEM_PAGO TYPE REF TO ZCL_ORDEM_PAGO.
*            CREATE OBJECT ORDEM_PAGO.
*            ORDEM_PAGO->SET_WITH_WRITE_PDF(
*              EXPORTING
*                I_BUKRS       = XBKPF-BUKRS
*                I_GJAHR       = XBKPF-GJAHR
*                I_AUGBL       = XBKPF-BELNR
*                I_WT_WITHCD   = PI_RETENCION-WT_WITHCD
*                I_WITHT       = PI_RETENCION-WITHT
*                I_BELNR_O     = PI_RETENCION-OPAGO
*                I_GJAHR_O     = PI_RETENCION-GJAHR
*                I_CERTIFICADO = V_CERTIFICADO
*                I_TEXTO_X     = LS_PDF_STRING_X ).
*        ENDCASE.
*
**          CALL FUNCTION 'SCMS_XSTRING_TO_BINARY'
**            EXPORTING
**              BUFFER     = LS_PDF_STRING_X
**            TABLES
**              BINARY_TAB = LT_PDF.
**
**          OPEN DATASET FILENAME FOR OUTPUT IN BINARY MODE.
**          IF SY-SUBRC IS INITIAL.
**            LOOP AT LT_PDF INTO LS_PDF.
**              TRANSFER LS_PDF TO FILENAME NO END OF LINE.
**            ENDLOOP.
**            CLOSE DATASET FILENAME.
**          ENDIF.
**
**          "Registrar que gerou
**          IF SY-SUBRC IS INITIAL.
**            WA_ZSDYT0053-CK_PRINT  = ABAP_TRUE.
**            MODIFY ZSDYT0053 FROM WA_ZSDYT0053.
**          ENDIF.
*
*      ENDIF.
*
**      ENDIF.
*
*    ENDIF.
*
*    CHECK SY-TCODE NE 'ZFIY0036'.
*
*    IF SY-BATCH IS INITIAL AND RB_PRNT EQ 'X' .
*
**--- Convert OTF to PDF
*      CALL FUNCTION 'CONVERT_OTF_2_PDF'
*        IMPORTING
*          BIN_FILESIZE           = V_BIN_FILESIZE
*        TABLES
*          OTF                    = ST_JOB_OUTPUT_INFO-OTFDATA
*          DOCTAB_ARCHIVE         = IT_DOCS
*          LINES                  = IT_LINES
*        EXCEPTIONS
*          ERR_CONV_NOT_POSSIBLE  = 1
*          ERR_OTF_MC_NOENDMARKER = 2
*          OTHERS                 = 3.
*      IF SY-SUBRC <> 0.
**      MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**      WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
**--- Download PDF to local PC
*      MOVE V_FULLPATH TO V_FILENAME.
*      CALL FUNCTION 'GUI_DOWNLOAD'
*        EXPORTING
*          BIN_FILESIZE            = V_BIN_FILESIZE
*          FILENAME                = V_FILENAME
*          FILETYPE                = 'BIN'
*        TABLES
*          DATA_TAB                = IT_LINES
*        EXCEPTIONS
*          FILE_WRITE_ERROR        = 1
*          NO_BATCH                = 2
*          GUI_REFUSE_FILETRANSFER = 3
*          INVALID_TYPE            = 4
*          NO_AUTHORITY            = 5
*          UNKNOWN_ERROR           = 6
*          HEADER_NOT_ALLOWED      = 7
*          SEPARATOR_NOT_ALLOWED   = 8
*          FILESIZE_NOT_ALLOWED    = 9
*          HEADER_TOO_LONG         = 10
*          DP_ERROR_CREATE         = 11
*          DP_ERROR_SEND           = 12
*          DP_ERROR_WRITE          = 13
*          UNKNOWN_DP_ERROR        = 14
*          ACCESS_DENIED           = 15
*          DP_OUT_OF_MEMORY        = 16
*          DISK_FULL               = 17
*          DP_TIMEOUT              = 18
*          FILE_NOT_FOUND          = 19
*          DATAPROVIDER_EXCEPTION  = 20
*          CONTROL_FLUSH_ERROR     = 21
*          OTHERS                  = 22.
*
*      IF SY-SUBRC = 0.
**          MESSAGE 'Se han grabado los Certificados de Retencion' TYPE 'S'.
**        ELSE.
***         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
***            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*      ENDIF.
*
*      SELECT SINGLE VALOR
*        INTO V_ENDERECO
*        FROM ZPARAMETROS
*       WHERE NOME_PARAMETRO = 'ENDERECO_PDF_PAGO' .
*
*      IF SY-SUBRC IS INITIAL.
**--- Download PDF para servidor
*        CONCATENATE V_ENDERECO V_FILENAME INTO V_FILENAME_S.
*        "MOVE V_FULLPATH TO V_FILENAME.
*        CALL FUNCTION 'GUI_DOWNLOAD'
*          EXPORTING
*            BIN_FILESIZE            = V_BIN_FILESIZE
*            FILENAME                = V_FILENAME_S
*            FILETYPE                = 'BIN'
*          TABLES
*            DATA_TAB                = IT_LINES
*          EXCEPTIONS
*            FILE_WRITE_ERROR        = 1
*            NO_BATCH                = 2
*            GUI_REFUSE_FILETRANSFER = 3
*            INVALID_TYPE            = 4
*            NO_AUTHORITY            = 5
*            UNKNOWN_ERROR           = 6
*            HEADER_NOT_ALLOWED      = 7
*            SEPARATOR_NOT_ALLOWED   = 8
*            FILESIZE_NOT_ALLOWED    = 9
*            HEADER_TOO_LONG         = 10
*            DP_ERROR_CREATE         = 11
*            DP_ERROR_SEND           = 12
*            DP_ERROR_WRITE          = 13
*            UNKNOWN_DP_ERROR        = 14
*            ACCESS_DENIED           = 15
*            DP_OUT_OF_MEMORY        = 16
*            DISK_FULL               = 17
*            DP_TIMEOUT              = 18
*            FILE_NOT_FOUND          = 19
*            DATAPROVIDER_EXCEPTION  = 20
*            CONTROL_FLUSH_ERROR     = 21
*            OTHERS                  = 22.
*        IF SY-SUBRC = 0.
**            MESSAGE 'Se han grabado todas las OP' TYPE 'S'.
**          ELSE.
**            MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
**               WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
*        ENDIF.
*
*      ENDIF.
*
*      IF RB_PRNT EQ 'X'.
*        "IMPRESSÃO -- Para mandar para impressora tenho que gerar novamente com o campo getotf em branco
*        LS_CONTROL-GETOTF  = ''.
*        CALL FUNCTION WGC_SMARTFORM
*          EXPORTING
*            USER_SETTINGS        = ' '
*            CONTROL_PARAMETERS   = LS_CONTROL
*            OUTPUT_OPTIONS       = LS_OPTIONS
*            GV_CHK_FECHA_IMP     = ''
*            GS_RETENCION         = PI_RETENCION
*            V_FLAG               = W_COPIA
*          IMPORTING
*            DOCUMENT_OUTPUT_INFO = ST_DOCUMENT_OUTPUT_INFO
*            JOB_OUTPUT_INFO      = ST_JOB_OUTPUT_INFO
*            JOB_OUTPUT_OPTIONS   = ST_JOB_OUTPUT_OPTIONS
*          TABLES
*            GT_RETENCION         = T_RET_POS
*          EXCEPTIONS
*            FORMATTING_ERROR     = 1
*            INTERNAL_ERROR       = 2
*            SEND_ERROR           = 3
*            USER_CANCELED        = 4
*            OTHERS               = 5.
*
*        IF SY-SUBRC <> 0.
*
*          MESSAGE ID SY-MSGID TYPE SY-MSGTY
*          NUMBER SY-MSGNO
*            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
*        ELSE.
**         --- Download PDF to local PC
*          MOVE V_FULLPATH TO V_FILENAME.
*          CALL FUNCTION 'GUI_DOWNLOAD'
*            EXPORTING
*              BIN_FILESIZE            = V_BIN_FILESIZE
*              FILENAME                = V_FILENAME
*              FILETYPE                = 'BIN'
*            TABLES
*              DATA_TAB                = IT_LINES
*            EXCEPTIONS
*              FILE_WRITE_ERROR        = 1
*              NO_BATCH                = 2
*              GUI_REFUSE_FILETRANSFER = 3
*              INVALID_TYPE            = 4
*              NO_AUTHORITY            = 5
*              UNKNOWN_ERROR           = 6
*              HEADER_NOT_ALLOWED      = 7
*              SEPARATOR_NOT_ALLOWED   = 8
*              FILESIZE_NOT_ALLOWED    = 9
*              HEADER_TOO_LONG         = 10
*              DP_ERROR_CREATE         = 11
*              DP_ERROR_SEND           = 12
*              DP_ERROR_WRITE          = 13
*              UNKNOWN_DP_ERROR        = 14
*              ACCESS_DENIED           = 15
*              DP_OUT_OF_MEMORY        = 16
*              DISK_FULL               = 17
*              DP_TIMEOUT              = 18
*              FILE_NOT_FOUND          = 19
*              DATAPROVIDER_EXCEPTION  = 20
*              CONTROL_FLUSH_ERROR     = 21
*              OTHERS                  = 22.
*
*          IF SY-SUBRC = 0.
**               MESSAGE 'Se han grabado los Certificados de Retencion' TYPE 'S'.
**             ELSE.
**     *         MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
**     *            WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*          ENDIF.
*
*        ENDIF.
*
*      ENDIF.
*
*    ENDIF.
*
*  ENDIF.
*
** ENDDO.
*
*ENDFORM.                    "f_llamo_smartforms

*&---------------------------------------------------------------------*
*&      Form  START_SAPSCRIPT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM start_sapscript.

*  CALL FUNCTION 'START_FORM'
**(del) exporting form      = 'J_1A_F015_WTHCTF'
*       EXPORTING form      = xt059p-tdform
*                 startpage = 'PAGE1'.

ENDFORM.                    " START_SAPSCRIPT


*&---------------------------------------------------------------------*
*&      Form  f_completo_st_RETENCION
*&---------------------------------------------------------------------*
FORM f_completo_st_retencion USING    pi_input  TYPE fiwtform_01
                                      pi_codtab TYPE ty_codtab
                             CHANGING po_ret    TYPE zfiys_retencion_cab.

  DATA: tl_t059minmax  TYPE STANDARD TABLE OF t059minmax,
        stl_t059minmax TYPE t059minmax.

* PERFORM f_logo CHANGING st_retencion-form.

  SELECT *
    FROM t059minmax
    INTO TABLE tl_t059minmax
    WHERE land1     EQ t001-land1
      AND witht     EQ pi_codtab-witht
      AND wt_withcd EQ pi_codtab-wt_withcd
      AND waers     EQ pi_input-waers.

  IF sy-subrc EQ 0.
    SORT tl_t059minmax BY wt_date DESCENDING.
    READ TABLE tl_t059minmax INTO stl_t059minmax INDEX 1.
    IF sy-subrc EQ 0.
      MOVE stl_t059minmax-wt_wtminb TO po_ret-mni .
    ENDIF.
  ELSE.
    CLEAR: po_ret-mni.
  ENDIF.
* Texto
  SELECT SINGLE text40
    FROM t059zt
    INTO po_ret-text3
    WHERE land1     = t001-land1
      AND witht     = pi_codtab-witht
      AND wt_withcd = pi_codtab-wt_withcd
      AND spras     = 'S'.

* Texto
  SELECT SINGLE text40
    FROM t059ot
    INTO po_ret-text2
    WHERE land1    = t001-land1
      AND wt_qscod = pi_input-qscod
      AND spras    = 'S'.


  SELECT SINGLE qsatz
    FROM t059z
    INTO  po_ret-alicuota
    WHERE land1     = t001-land1
      AND witht     = pi_codtab-witht
      AND wt_withcd = pi_codtab-wt_withcd.

  IF pi_input-ctnumber IS INITIAL.
    MOVE: codtab-ctnumber TO pi_input-ctnumber.
  ENDIF.

  IF pi_codtab-lifnr IS NOT INITIAL.
    SELECT SINGLE j_1kfrepre
      FROM lfa1
        INTO po_ret-j_1kfrepre
          WHERE lifnr EQ pi_codtab-lifnr.

*    IF sy-subrc EQ 0.
*      SELECT SINGLE descrip
*        FROM zfi_t_cod_afip
*          INTO po_ret-desccodafip
*            WHERE j_1kfrepre EQ po_ret-j_1kfrepre.
*    ENDIF.

  ENDIF.

  MOVE:
        pi_input-name1_1    TO po_ret-name1_1,
        pi_codtab-witht     TO po_ret-witht,
        pi_codtab-lifnr     TO po_ret-lifnr,
        pi_codtab-wt_withcd TO po_ret-wt_withcd,
        pi_input-stras_1    TO po_ret-stras_1,
        pi_input-pstlz_1    TO po_ret-pstlz_1,
*       pi_input-ort01_1    TO po_ret-ort01_1,
        w_xort01            TO po_ret-ort01_1,
        pi_input-pfach_1    TO po_ret-pfach_1,
        pi_input-stcdt_1    TO po_ret-stcdt_1,
        pi_input-stcd1_1    TO po_ret-stcd1_1,
        pi_input-stcd2_1    TO po_ret-stcd3_1,
        pi_input-stcd3_1    TO po_ret-stcd2_1,
        pi_input-anred_2    TO po_ret-anred_2,
        pi_input-name1_2    TO po_ret-name1_2,
        pi_input-stras_2    TO po_ret-stras_2,
        pi_input-pstlz_2    TO po_ret-pstlz_2,
        pi_input-ort01_2    TO po_ret-ort01_2,
        pi_input-pfach_2    TO po_ret-pfach_2,
        pi_input-stcdt_2    TO po_ret-stcdt_2,
        pi_input-stcd1_2    TO po_ret-stcd1_2,
        pi_input-stcd2_2    TO po_ret-stcd2_2,
        pi_input-ctnumber   TO po_ret-ctnumber,
        pi_input-budat      TO po_ret-augdt,
        pi_input-bldat      TO po_ret-fecha,
        pi_input-text3      TO po_ret-text1,
        pi_input-wt_qsshb   TO po_ret-imp_base,
        pi_input-wt_qbshb   TO po_ret-retencion,
        pi_input-wt_accbs   TO po_ret-base_acum,
        pi_input-qscod      TO po_ret-cod_fip,
        po_ret-retencion    TO po_ret-imp_tot_ret,
        pi_input-waers      TO po_ret-waers,
        bkpf-gjahr          TO po_ret-gjahr,
        bkpf-belnr          TO po_ret-opago.

  CONCATENATE pi_input-isnr2(4) '-'
              pi_input-offnum2(8)
        INTO  po_ret-comprobante.

  IF pi_input-j_1aformat NE 'N'.
    IF pi_input-offnum NE space.
      CONCATENATE pi_input-isnr '-' pi_input-offnum INTO po_ret-certificado.
    ENDIF.
  ELSE.
*   concatenate pi_input-isnr '-' pi_input-prdate+6(4) '-' po_RET-CTNUMBER into po_RET-COMPROBANTE.
  ENDIF.

ENDFORM.                    " f_completo_st_RETENCION

*&---------------------------------------------------------------------*
*&      Form  F_LOGO
*&---------------------------------------------------------------------*
FORM f_logo CHANGING pi_form.

*  SELECCIONA LOGO
*  SELECT SINGLE name1
*  FROM zfi_t_logo
*  INTO pi_form
*  WHERE bukrs EQ t001-bukrs .

ENDFORM.                    " F_LOGO
*&---------------------------------------------------------------------*
*&      Form  FILL_GENERAL_FIELDS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM fill_general_fields.
  CLEAR fiwtform_01.

* Window cc_data1
  fiwtform_01-name1_1 = xsadr-name1.
  fiwtform_01-stras_1 = xsadr-stras.
  fiwtform_01-pstlz_1 = xsadr-pstlz.
  fiwtform_01-ort01_1 = xsadr-ort01.

*  *****LONGITUD DE ORT01 Invalida.
  w_xort01 = xsadr-ort01.

  fiwtform_01-pfach_1 = xsadr-pfach.
  fiwtform_01-text1   = xt001-fitptxt.

* Window cc_data1
  IF s_pdate IS INITIAL.
    fiwtform_01-prdate  = bkpf-budat.
  ELSE.
    fiwtform_01-prdate  = s_pdate.
  ENDIF.

  fiwtform_01-stcdt_1  = xt001-stcdt.
  fiwtform_01-stcd1_1  = xt001-stcd1.
  fiwtform_01-stcd2_1  = xt001-stcd2.
  fiwtform_01-stcd3_1 =  xt001-stcd3.
  fiwtform_01-hkont    = xlfa1-lifnr.

* Window v_data
  fiwtform_01-anred_2  = xlfa1-anred.
  fiwtform_01-name1_2  = xlfa1-name1.
  fiwtform_01-stras_2  = xlfa1-stras.
  fiwtform_01-pstlz_2  = xlfa1-pstlz.
  fiwtform_01-ort01_2  = xlfa1-ort01.
  fiwtform_01-pfach_2  = xlfa1-pfach.
  fiwtform_01-stcdt_2  = xlfa1-stcdt.
  fiwtform_01-stcd1_2  = xlfa1-stcd1.
  fiwtform_01-stcd2_2  = xlfa1-stcd2.
  fiwtform_01-stcd3_2 = xlfa1-stcd3.

  fiwtform_01-text2    = xlfa1-fitptxt.

* Window add_data
  fiwtform_01-j_1aformat  = xt059p-j_1aformat.
*      Corr. 3645505
*  j_1ai02-isnr    = j_1a104-certno(4).
*  j_1ai02-offnum  = j_1a104-certno+4(8).
  fiwtform_01-isnr     = bkpf-brnch.
* fiwtform_01-ctnumber = codtab-ctnumber.
  fiwtform_01-offnum = codtab-ctnumber+2(8).
*
  fiwtform_01-text3    = xt059p-text.
  fiwtform_01-regio    = xt059p-regio.
  fiwtform_01-text4    = xt005u-bezei.

* Window copy_no
  fiwtform_01-copyno  = copy_no.

* Window MAIN
*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'HEADER'.

ENDFORM.                    " FILL_GENERAL_FIELDS

*&---------------------------------------------------------------------*
*&      Form  PRINT_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_clrdtab.
* print cleared documents
  CLEAR xblart.
**Note 1009246 Begins
*  LOOP AT CLRDTAB WHERE EMPFG = CODTAB-EMPFG
*                  AND   WITHT = CODTAB-WITHT.
  LOOP AT clrdtab WHERE lifnr = xlifnr
                   AND empfg = codtab-empfg
                   AND   witht = codtab-witht.
**Note 1009246 Ends

    IF clrdtab-blart NE xblart.
      xblart = clrdtab-blart.
      PERFORM read_off_doc_type.
    ENDIF.
*    at new blart.
*      perform read_off_doc_type.
*    endat.

* --> Begin of OSS                                         Note 393173
* --> Main changes included in one form
    PERFORM read_withheld_amount .
* <-- End   of OSS                                         Note 393173

    fiwtform_01-isnr2   = clrdtab-xblnr(4).
    fiwtform_01-prtchr2 = clrdtab-xblnr+4(1).
    fiwtform_01-offnum2 = clrdtab-xblnr+5(8).
*   j_1ai02-isnr2   = clrdtab-j_1aisnr.
*   j_1ai02-prtchr2 = clrdtab-j_1aprtchr.
*   j_1ai02-offnum2 = clrdtab-j_1aoffnum.
    fiwtform_01-belnr   = clrdtab-belnr.
    fiwtform_01-gjahr   = clrdtab-gjahr.
    fiwtform_01-bldat   = clrdtab-bldat.
    fiwtform_01-oftp    = xoftp-j_1aoftp.
    fiwtform_01-text5   = xoftp-text30.

*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'CLRD_DOCM'.

  ENDLOOP.

ENDFORM.                    " PRINT_CLRDTAB

*&---------------------------------------------------------------------*
*&      Form  READ_OFF_DOC_TYPE
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_off_doc_type.
  CHECK: xoftp-doccls NE clrdtab-blkls OR
*         xoftp-j_1aprtchr ne clrdtab-j_1aprtchr.
         xoftp-j_1aprtchr NE clrdtab-xblnr+4(1).

  CLEAR: j_1aotdetr, j_1aoftpt, xoftp.

  READ TABLE xoftp WITH KEY doccls = clrdtab-blkls
*                           j_1aprtchr = clrdtab-j_1aprtchr.      " !!!
                            j_1aprtchr = clrdtab-xblnr+4(1).

  CHECK sy-subrc NE 0.

*  SELECT SINGLE * FROM J_1AOTDET WHERE J_1ADOCCLS = CLRDTAB-BLKLS
*                  and   j_1aprtchr = clrdtab-j_1aprtchr.         " !!!
*                  AND   J_1APRTCHR = CLRDTAB-XBLNR+4(1).

  SELECT SINGLE * FROM  j_1aotdetr                         "Note 511483
                        WHERE land1      = t001-land1
                        AND   id_report  = sy-repid
                        AND   doccls     = clrdtab-blkls
                        AND   j_1aprtchr = clrdtab-xblnr+4(1).


  SELECT SINGLE * FROM j_1aoftpt WHERE spras    = 'S'
                                 AND   j_1aoftp = j_1aotdetr-j_1aoftp.

  MOVE-CORRESPONDING: j_1aotdetr TO xoftp,                 "Note 511483
                      j_1aoftpt  TO xoftp.

  APPEND xoftp.
ENDFORM.                    " READ_OFF_DOC_TYPE

*&---------------------------------------------------------------------*
*&      Form  PRINT_CODTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_codtab.
  fiwtform_01-qscod    = codtab-qscod.
  fiwtform_01-text5    = codtab-qsbez.
* --> Begin of OSS                                          Note 393173
* FIWTFORM_01-WT_QBSHB = CODTAB-QBSHB.
  IF  NOT codtab-wwrbtr IS INITIAL .
    fiwtform_01-wt_qbshb =
              codtab-qbshb + codtab-wwrbtr .
    fiwtform_01-wt_alwt  = 'X' .
  ELSE .
    fiwtform_01-wt_qbshb = codtab-qbshb.
  ENDIF .
* <-- End   of OSS                                          Note 393173
  fiwtform_01-wt_qsshb = codtab-qsshb.
  fiwtform_01-waers    = bkpf-waers.

  IF t001-waers = bkpf-waers.
    fiwtform_01-wt_accbs = codtab-accbs.
    fiwtform_01-wt_accwt = codtab-accwt.
    fiwtform_01-waers2   = bkpf-waers.
  ENDIF.

* --> Begin of OSS                                          Note 393173
  IF NOT codtab-wdmbtr IS INITIAL .
    fiwtform_01-wt_wdmbtr     = codtab-wdmbtr.
    fiwtform_01-wt_wwrbtr     = codtab-wwrbtr.
  ENDIF .
* <-- End   of OSS                                          Note 393173

*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'LINES_OWC'.

  IF t001-waers NE bkpf-waers.
    CLEAR: fiwtform_01-qscod, fiwtform_01-text5.

    fiwtform_01-wt_accbs = codtab-accbs.
    fiwtform_01-wt_accwt = codtab-accwt.
    fiwtform_01-wt_qsshb = codtab-qsshh.
* --> Begin of OSS                                          Note 393173
*    FIWTFORM_01-WT_QBSHB = CODTAB-QBSHH.
    IF  NOT codtab-wwrbtr IS INITIAL .
      fiwtform_01-wt_qbshb =
                 codtab-qbshh + codtab-wdmbtr .
      fiwtform_01-wt_alwt  = 'X' .
    ELSE .
      fiwtform_01-wt_qbshb = codtab-qbshh.
    ENDIF .
* <-- End   of OSS                                          Note 393173
    fiwtform_01-waers    = t001-waers.
    fiwtform_01-waers2   = t001-waers.

*    CALL FUNCTION 'WRITE_FORM'
*      EXPORTING
*        element = 'LINES_OWC'.
  ENDIF.
ENDFORM.                    " PRINT_CODTAB

*&---------------------------------------------------------------------*
*&      Form  PRINT_FINAL_TEXT
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_final_text.
  CLEAR: fiwtform_01-text6, spell.

  CALL FUNCTION 'SPELL_AMOUNT'
    EXPORTING
      language = 'S'
      currency = t001-waers
      amount   = codtab-qbshh
      filler   = space
    IMPORTING
      in_words = spell.

  SET LOCALE LANGUAGE 'S'.
  TRANSLATE spell-word TO LOWER CASE.

  fiwtform_01-text5 = tcurt-ltext.
  fiwtform_01-belnr = bkpf-belnr.
  fiwtform_01-budat = bkpf-budat.
*  fiwtform_01-sign  = s_sign.
*  fiwtform_01-legal = s_legal.

  IF bkpf-budat+4(2) < 16.
    fiwtform_01-fortn = 1.
    fiwtform_01-text6 = text-p01.
  ELSE.
    fiwtform_01-fortn = 2.
    fiwtform_01-text6 = text-p02.
  ENDIF.

*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'TEXT'.

ENDFORM.                    " PRINT_FINAL_TEXT

*&---------------------------------------------------------------------*
*&      Form  READ_CLRD_DOCUMENTS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_clrd_documents.

  LOOP AT paytab.
    IF xkunnr NE space.
      SELECT * FROM bsad WHERE bukrs EQ bkpf-bukrs
                         AND   kunnr EQ xkunnr
                         AND   augdt EQ paytab-augdt
                         AND   augbl EQ bkpf-belnr
                         AND   belnr NE bkpf-belnr.

        CLEAR bsak.
        MOVE-CORRESPONDING bsad TO bsak.
        bsak-lifnr = bsad-kunnr.
        PERFORM create_clrdtab.
      ENDSELECT.

      IF sy-subrc = 0.
        CLEAR xkunnr.
      ENDIF.
    ENDIF.

    AT NEW augdt.
      CLEAR no_clrd_docm.

      IF NOT itab_xlifnr IS INITIAL.
        SELECT * FROM bsak FOR ALL ENTRIES IN itab_xlifnr WHERE bukrs EQ bkpf-bukrs
                         AND   lifnr EQ itab_xlifnr-lifnr
                          AND   umsks EQ paytab-umsks
                          AND   umskz EQ paytab-umskz
                          AND   augdt EQ paytab-augdt
                          AND   augbl EQ bkpf-belnr
                          AND   belnr NE bkpf-belnr.
          PERFORM create_clrdtab.
        ENDSELECT.
      ENDIF.
**Note 1116475 End

*      ------------------------------------- No cleared document found!
      IF sy-subrc NE 0.
        no_clrd_docm = 'X'.
      ENDIF.
    ENDAT.

*   check: no_clrd_docm   ne space and
*          paytab-xzahl ne space.

    CHECK: ( paytab-xzahl NE space
         AND no_clrd_docm NE space ).
*                  Corr. 3645505
*         or ( paytab-umsks eq 'A'            " dp clearings in payments
*         and not paytab-rebzg is initial ).

*   -------------------------------------------------------------------
    clrdtab-empfg = paytab-empfg.
    clrdtab-witht = paytab-witht.

    IF NOT paytab-rebzg IS INITIAL.
      PERFORM read_document_header USING paytab-rebzg paytab-rebzj.
      MOVE-CORRESPONDING xbkpf TO clrdtab.
      SELECT SINGLE * FROM t003 WHERE blart = xbkpf-blart.
      clrdtab-blkls = t003-blkls.

      COLLECT clrdtab.
    ELSE.
      MOVE-CORRESPONDING bkpf TO clrdtab.
      SELECT SINGLE * FROM t003 WHERE blart = bkpf-blart.
      clrdtab-blkls = t003-blkls.
*     -------------------------------- Are there down payment requests?
      IF paytab-umsks = 'A'.
        SELECT * FROM bsak
                WHERE bukrs = bkpf-bukrs
                AND   lifnr = xlifnr
                AND   umsks = 'A'
                AND   umskz = 'F'
                AND   augdt = bkpf-budat
                AND   augbl = bkpf-belnr
*                and   zuonr = ???
                AND   belnr NE bkpf-belnr.
*                and   gjahr = ???
          clrdtab-belnr = bsak-belnr.
*          clear clrdtab-wt_qbshh.
          COLLECT clrdtab.
        ENDSELECT.
        IF NOT sy-subrc = 0.
          COLLECT clrdtab.
        ENDIF.
      ELSE.

        COLLECT clrdtab.
      ENDIF.
    ENDIF.

  ENDLOOP.                                                  " Of paytab
ENDFORM.                    " READ_CLRD_DOCUMENTS

*&---------------------------------------------------------------------*
*&      Form  CREATE_CLRDTAB
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM create_clrdtab.
  CLEAR: clrdtab.   ", j_1awtpkey.

*  if bsak-xcpdd ne space.                                 " P45K039668
  IF bsak-xcpdd NE space AND                               " P45K039668
     lfa1-xcpdk NE space.                                  " P45K039668

    PERFORM read_cpd_data USING bsak-belnr bsak-buzei bsak-gjahr.
    CHECK xbsec-empfg = paytab-empfg.

    MOVE-CORRESPONDING xbsec TO clrdtab.
  ENDIF.

  MOVE-CORRESPONDING: bsak TO clrdtab.   ",
*                      bsak to j_1awtpkey.
*------------------------------Make sure the internal table contains the
*-----------------------------------official document class in Argentina
  SELECT SINGLE * FROM t003 WHERE blart = clrdtab-blart. " Note 200180
  clrdtab-blkls = t003-blkls.                            " Note 200180

* -------------------- Is there a down payment or down payment request?
  IF bsak-rebzt = 'U'.
*   --- Switch to that line in the DPC which points to the down payment
    bsak-buzei = bsak-buzei - 1.
    SELECT SINGLE * FROM bsak INTO downpay         " <--- Corr. 3645505
           WHERE bukrs = bsak-bukrs
           AND   lifnr = bsak-lifnr
           AND   umsks = 'A'
           AND   umskz = 'A'
           AND   augdt = bsak-augdt
           AND   augbl = bsak-belnr
           AND   zuonr = bsak-zuonr
           AND   gjahr = bsak-gjahr
           AND   belnr = bsak-belnr
           AND   buzei = bsak-buzei.

* ---------------------------------------------------------------------
*                     |                             |
*                     V                             V
*                           Corr. 3645505
* ---------------------------------------------------------------------
    IF sy-subrc = 0 AND                   " Yes, there is a downpayment
       downpay-rebzg NE space AND
       downpay-rebzt = 'A'.
*      -------------------------------------- read downpayment document
      SELECT * FROM bsak INTO downpay
         WHERE bukrs = downpay-bukrs
         AND   lifnr = downpay-lifnr
         AND   umsks = 'A'
         AND   umskz = 'A'
         AND   augdt = downpay-augdt
         AND   augbl = downpay-belnr
         AND   zuonr = downpay-zuonr
*        and   gjahr = ???
         AND   belnr = downpay-rebzg
*        and   buzei = ???
         AND   dmbtr = downpay-dmbtr
         AND   wrbtr = downpay-wrbtr
         AND   qsshb = downpay-qsshb
         AND   qbshb = downpay-qbshb.
        APPEND downpay.
      ENDSELECT.
*      ------- check whether downpayment line has already been selected
      LOOP AT downpay.
        PERFORM check_downpay_already_selected.
        IF sy-subrc = 0.                " It has already been selected
          DELETE downpay.
        ELSE.                           " It has not yet been selected
          APPEND downpay TO already_selected.
          EXIT.
        ENDIF.
      ENDLOOP.
      IF sy-subrc = 0.
        MOVE-CORRESPONDING downpay TO bsak.
        MOVE-CORRESPONDING downpay TO downpay_struc.
        CLEAR   downpay.
        REFRESH downpay.
      ENDIF.
    ENDIF.
  ENDIF.

* -------------------------------------------- Read downpayment request
  SELECT * FROM bsak INTO downpay
    WHERE bukrs = bsak-bukrs
    AND   lifnr = bsak-lifnr
    AND   umsks = 'A'
    AND   umskz = 'F'
    AND   augdt = bsak-budat
    AND   augbl = bsak-belnr             " Clearing doc. is downpayment
    AND   belnr NE bsak-belnr
    AND   zuonr = bsak-zuonr
*   and   gjahr = bsak-gjahr
*   and   buzei = .
    AND   dmbtr = bsak-dmbtr
    AND   wrbtr = bsak-wrbtr.
    APPEND downpay.
  ENDSELECT.
* ------------ Check whether downpayment line has already been selected
  LOOP AT downpay.
    PERFORM check_downpay_already_selected.
    IF sy-subrc = 0.                     " It has already been selected
      DELETE downpay.
    ELSE.                                " It has not yet been selected
      APPEND downpay TO already_selected.
      EXIT.
    ENDIF.
  ENDLOOP.
  IF sy-subrc = 0.
    MOVE-CORRESPONDING downpay TO bsak.
    CLEAR   downpay.
    REFRESH downpay.
  ENDIF.

* ------------------------------------ Get document header informations
  PERFORM read_document_header USING bsak-belnr bsak-gjahr.
  MOVE-CORRESPONDING xbkpf TO clrdtab.

*   ----------------- Read withholding tax data of downpayment document
  IF downpay_struc-bukrs NE space AND
     downpay_struc-belnr NE space AND
     downpay_struc-gjahr NE space AND
     downpay_struc-buzei NE space.
    MOVE-CORRESPONDING downpay_struc TO bsak.
    CLEAR downpay_struc.
  ENDIF.
  CLEAR: clrdtab-witht.

  SELECT * FROM with_item
           WHERE bukrs     EQ bsak-bukrs
*             and   j_1awtkey eq j_1awtkey
           AND   belnr     EQ bsak-belnr
           AND   gjahr     EQ bsak-gjahr
           AND   buzei     EQ bsak-buzei
           AND   witht     IN s_witht
           AND   wt_withcd NE space
           AND   wt_stat   EQ space.
*            and   j_1astat  eq space.

    PERFORM read_withhld_code.
*     Only WT postings that lead to a reduction of the invoice amount
*     are reported to the vendor. Thus grossing up posting is excluded.
    CHECK xt059z-wt_posin = '1'.

    clrdtab-witht    = with_item-witht.
*      clrdtab-wthcd    = with_item-wt_withcd.
*      clrdtab-wt_qbshh = with_item-wt_qbshh * -1.
    COLLECT clrdtab.
  ENDSELECT.

ENDFORM.                                            " of create_clrdtab


*&---------------------------------------------------------------------*
*&      Form  PRINT_TOTALS
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM print_totals.
  fiwtform_01-wt_accbs = codtab-accbs.
  fiwtform_01-wt_accwt = codtab-accwt.
  fiwtform_01-wt_qsshb = codtab-qsshb.
  fiwtform_01-wt_qbshb = codtab-qbshb.
  fiwtform_01-waers    = bkpf-waers.
  fiwtform_01-wt_qsshh = codtab-qsshh.
  fiwtform_01-wt_qbshh = codtab-qbshh.
  fiwtform_01-waers2   = t001-waers.

*  CALL FUNCTION 'WRITE_FORM'
*    EXPORTING
*      element = 'TOTAL_OWC'.

ENDFORM.                    " PRINT_TOTALS
*&---------------------------------------------------------------------*
*&      Form  READ_DOCUMENT_HEADER
*&---------------------------------------------------------------------*
*       text                                                           *
*----------------------------------------------------------------------*
FORM read_document_header USING f_belnr f_gjahr.

  CHECK xbkpf-bukrs NE bkpf-bukrs OR
        xbkpf-belnr NE f_belnr    OR
        xbkpf-gjahr NE f_gjahr.

  CLEAR: xbkpf, *bkpf.

  READ TABLE xbkpf WITH KEY bukrs = bkpf-bukrs
                            belnr = f_belnr
                            gjahr = f_gjahr.

  CHECK sy-subrc NE 0.

  SELECT SINGLE * FROM bkpf INTO *bkpf
                 WHERE bukrs = bkpf-bukrs
                 AND   belnr = f_belnr
                 AND   gjahr = f_gjahr.

*  if *bkpf-j_1aoffnum is initial and  " xblnr now includes j_1aoffnum!
*     *bkpf-xblnr ne space.
*     xoffnum = *bkpf-xblnr.
*     *bkpf-j_1aoffnum = xoffnum.
*  endif.

  MOVE-CORRESPONDING *bkpf TO xbkpf.
  MOVE bkpf-belnr TO xbkpf-augbl.
  APPEND xbkpf.

ENDFORM.                    " READ_DOCUMENT_HEADER

*&---------------------------------------------------------------------*
*&      Form  VERIFY_J2AC
*&---------------------------------------------------------------------*
*       Check for old documents from 2.1 that might be interpreted
*       wrongly by 3.0 reports
*----------------------------------------------------------------------*
FORM verify_j2ac CHANGING p_subrc.
*tables: *bkpf.

  CLEAR: p_subrc.
  IF bkpf-tcode = 'J2AC'.
    p_subrc = -4.
  ELSE.
    IF bkpf-stblg NE space.
      SELECT SINGLE * FROM bkpf INTO *bkpf WHERE
                        bukrs = bkpf-bukrs AND
                        belnr = bkpf-stblg AND
                        gjahr = bkpf-stjah.
      IF sy-subrc EQ 0 AND *bkpf-tcode = 'J2AC'.
        p_subrc = -4.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    " VERIFY_J2AC

*                       Corr. 3645505
*&---------------------------------------------------------------------*
*&      Form  check_downpay_already_selected
*&---------------------------------------------------------------------*
*       Check whether downpayment of downpayment request has already
*       been selected
*----------------------------------------------------------------------*
FORM check_downpay_already_selected.
  READ TABLE already_selected WITH KEY
    bukrs = downpay-bukrs
    lifnr = downpay-lifnr
    umsks = downpay-umsks
    umskz = downpay-umskz
    augdt = downpay-augdt
    augbl = downpay-augbl
    zuonr = downpay-zuonr
    gjahr = downpay-gjahr
    belnr = downpay-belnr
    buzei = downpay-buzei.
ENDFORM.                    "CHECK_DOWNPAY_ALREADY_SELECTED

* --> Begin  of OSS                                         Note 393173
*&---------------------------------------------------------------------*
*&      Form  READ_WITHHELD_AMOUNT
*&---------------------------------------------------------------------*
*       text  OSS note 393173
*             All coding in the present form should be included as
*             part of the implementation of the mentioned note
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM read_withheld_amount.
  CHECK no_clrd_docm IS INITIAL .
  CHECK awheld_done IS INITIAL .
  READ TABLE x_t059p WITH KEY land1 = t001-land1
                              witht = clrdtab-witht
       BINARY SEARCH .

* Only if document include Withholding Types with Already amount
  CHECK sy-subrc IS INITIAL .
  SELECT * FROM with_item
          WHERE bukrs = bkpf-bukrs
            AND belnr = clrdtab-belnr
            AND gjahr = clrdtab-gjahr
            AND witht = clrdtab-witht
            AND wt_wdmbtr NE 0 .
    codtab-wdmbtr = codtab-wdmbtr + ( with_item-wt_wdmbtr * -1 ).
    codtab-wwrbtr = codtab-wwrbtr + ( with_item-wt_wwrbtr * -1 ).
  ENDSELECT .

  MODIFY codtab TRANSPORTING
     wdmbtr wwrbtr accbs accwt
   WHERE
     empfg    EQ codtab-empfg     AND
     ctnumber EQ codtab-ctnumber  AND
     witht    EQ codtab-witht     .

ENDFORM.                               " READ_WITHHELD_AMOUNT

**&---------------------------------------------------------------------*
**&      Form  ENVIAR_EMAIL
**&---------------------------------------------------------------------*
**       text
**----------------------------------------------------------------------*
**      -->P_FILENAME  text
**      -->P_PI_RETENCION  text
**----------------------------------------------------------------------*
*FORM ENVIAR_EMAIL  USING    P_FILENAME      TYPE STRING
*                            P_RETENCION     TYPE ZFIYS_RETENCION_CAB
*                            LS_PDF_STRING_X TYPE XSTRING.
*
*  DATA: I_HTML_ENTRA      TYPE STRING,
*        LO_CREATE_MAIL    TYPE REF TO CL_CRM_EMAIL_DATA,
*        LS_MAIL_BODY      TYPE CRMS_EMAIL_MIME_STRUC,
*        LS_RECEP          TYPE CRMS_EMAIL_RECIPIENT,
*        LV_ACTIVITY       TYPE SYSUUID_X,
*        WA_LIFNR          TYPE LFA1,
*        WA_ADR6           TYPE ADR6,
*        MAIL_DESTINATARIO TYPE STRING.
*
*  SELECT SINGLE * INTO WA_LIFNR FROM LFA1 WHERE LIFNR EQ P_RETENCION-LIFNR.
*  CONCATENATE I_HTML_ENTRA '<html>'  INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<head>'  INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '</head>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<body>'  INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Estimado:' WA_LIFNR-NAME1 '</FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Tenemos el agrado de enviar  las novedades ocurridas  en su cuenta corriente.</FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>Este es un e-mail automático, por informaciones adicional por favor contactar:' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<a href="mailto:cuentas@amaggi.com.ar">cuentas@amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA SEPARATED BY SPACE.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma>CORDIALES SALUDOS</FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>AMAGGI ARGENTINA SA</FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.br">www.amaggi.com.br</a></FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma><a href="http://www.amaggi.com.ar">www.amaggi.com.ar</a></FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV align=left>&nbsp;</DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Antes de imprimir, piense en su responsabilidad con el MEDIO AMBIENTE !!!!!</FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '<DIV><FONT face=Tahoma color:#1F497D>Before printing, think about your responsibility for the ENVIRONMENT!!!</FONT></DIV>' INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '</body>'     INTO I_HTML_ENTRA.
*  CONCATENATE I_HTML_ENTRA '</html>'     INTO I_HTML_ENTRA.
*  MAIL_DESTINATARIO = PEMAIL.
*
*  IF PEMAIL IS INITIAL.
*    IF SY-SYSID EQ 'PRD'.
*      IF WA_LIFNR-ADRNR IS NOT INITIAL.
*        SELECT SINGLE * INTO WA_ADR6
*          FROM ADR6
*         WHERE ADDRNUMBER EQ WA_LIFNR-ADRNR.
*        IF SY-SUBRC IS INITIAL AND WA_ADR6-SMTP_ADDR IS NOT INITIAL.
*          MAIL_DESTINATARIO = WA_ADR6-SMTP_ADDR.
*        ENDIF.
*      ENDIF.
*    ELSE.
*      DATA: IT_BAPIRET2   TYPE TABLE OF BAPIRET2 WITH HEADER LINE,
*            IT_BAPIADSMTP TYPE TABLE OF BAPIADSMTP WITH HEADER LINE.
*
*      CALL FUNCTION 'BAPI_USER_GET_DETAIL'
*        EXPORTING
*          USERNAME = SY-UNAME
*        TABLES
*          RETURN   = IT_BAPIRET2
*          ADDSMTP  = IT_BAPIADSMTP.
*
*      READ TABLE IT_BAPIADSMTP INDEX 1.
*      IF SY-SUBRC IS INITIAL AND IT_BAPIADSMTP-E_MAIL IS NOT INITIAL.
*        MAIL_DESTINATARIO = IT_BAPIADSMTP-E_MAIL.
*      ENDIF.
*    ENDIF.
*  ENDIF.
*
*  SY-SUBRC = 1.
*  CHECK MAIL_DESTINATARIO IS NOT INITIAL.
*
*  CREATE OBJECT LO_CREATE_MAIL.
*
*  CLEAR: LO_CREATE_MAIL->SUBJECT.
*  LO_CREATE_MAIL->SUBJECT = 'Certificado de Retención de Impuestos'.
*
*  CLEAR LS_MAIL_BODY.
*  LS_MAIL_BODY-CONTENT_ASCII = I_HTML_ENTRA.
*  LS_MAIL_BODY-MIME_TYPE     = 'text/html'.
*  APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.
*
*  CLEAR LS_MAIL_BODY.
*  LS_MAIL_BODY-IS_ATTACHMENT = 'X'.
*  LS_MAIL_BODY-FILE_NAME     = P_FILENAME.
*  LS_MAIL_BODY-MIME_TYPE     = 'application/pdf'.
*  LS_MAIL_BODY-CONTENT_BIN   = LS_PDF_STRING_X.
*  APPEND  LS_MAIL_BODY TO LO_CREATE_MAIL->BODY.
*
*  CLEAR LS_RECEP.
*  LS_RECEP-ADDRESS = MAIL_DESTINATARIO.
*  APPEND LS_RECEP TO LO_CREATE_MAIL->TO.
*
*  CLEAR LS_RECEP.
*  LS_RECEP-NAME    = 'AMAGGI'.
*  LS_RECEP-ADDRESS = 'cuentas@amaggi.com.ar'.
*  MOVE LS_RECEP TO LO_CREATE_MAIL->FROM.
*
*  CALL METHOD CL_CRM_EMAIL_UTILITY_BASE=>SEND_EMAIL
*    EXPORTING
*      IV_MAIL_DATA       = LO_CREATE_MAIL
*    RECEIVING
*      EV_SEND_REQUEST_ID = LV_ACTIVITY.
*
*  COMMIT WORK.
*
*ENDFORM.
