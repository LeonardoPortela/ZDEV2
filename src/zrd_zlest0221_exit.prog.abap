*&---------------------------------------------------------------------*
*& Report  ZRD_ZLEST0221_EXIT
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
REPORT zrd_ZLEST0221_exit.

TYPES: BEGIN OF ty_rbkp_sel,
         lifnr TYPE rbkp-lifnr,
         xblnr TYPE rbkp-xblnr,
         stblg TYPE rbkp-stblg.
TYPES: END   OF ty_rbkp_sel.

TYPES: BEGIN OF ty_bkpf_sel,
         bukrs TYPE bkpf-bukrs,
         gjahr TYPE bkpf-gjahr,
         awkey TYPE bkpf-awkey.
TYPES: END   OF ty_bkpf_sel.

TYPES: BEGIN OF ty_select,
 line TYPE fieldname,
 END OF ty_select.

data: desc_EMPRESA    TYPE butxt.

data: t_rbkp_sel           TYPE TABLE OF ty_rbkp_sel,
      w_rbkp_sel           TYPE ty_rbkp_sel,
      t_bkpf_sel           TYPE TABLE OF ty_bkpf_sel,
      w_bkpf_sel           TYPE ty_bkpf_sel.
data t_where  TYPE TABLE OF ty_select WITH HEADER LINE.

FORM f_exit_ZLEST0221_0002 USING p_registro_manter TYPE any CHANGING p_error TYPE char01.

  DATA: wa_ZLEST0221 TYPE ZLEST0221.
  CLEAR:  wa_ZLEST0221.

  MOVE-CORRESPONDING p_registro_manter TO wa_ZLEST0221.

ENDFORM.

FORM f_exit_ZLEST0221_0003 CHANGING p_registro_manter TYPE any.

  DATA: wa_ZLEST0221 TYPE ZLEST0221.
  MOVE-CORRESPONDING p_registro_manter TO wa_ZLEST0221.

  MOVE-CORRESPONDING wa_ZLEST0221 TO p_registro_manter.

ENDFORM.

FORM f_exit_ZLEST0221_0004 CHANGING p_saida TYPE any.

  DATA: wl_ZLEST0221_out TYPE ZLEST0221_out,
        l_awkey    TYPE bkpf-awkey.


  CLEAR: wl_ZLEST0221_out.


MOVE-CORRESPONDING p_saida TO wl_ZLEST0221_out.
  CLEAR p_saida.

  IF sy-subrc IS INITIAL.

    SELECT lifnr, stcd1
    FROM lfa1
    INTO TABLE @DATA(t_lfa1)
   WHERE stcd1 = @wl_ZLEST0221_out-cnpj_transbordo.


  SORT t_lfa1 BY stcd1.
  DELETE ADJACENT DUPLICATES FROM t_lfa1
                        COMPARING stcd1.

   READ TABLE t_lfa1 INTO DATA(w_lfa1) WITH KEY stcd1 = wl_ZLEST0221_out-cnpj_transbordo
                      BINARY SEARCH.

    if sy-subrc = 0.
    w_rbkp_sel-lifnr     = w_lfa1-lifnr.
    "w_rbkp_sel-xblnr     = wl_ZLEST0221_out-nfps.
    w_rbkp_sel-stblg     = abap_off.
    APPEND w_rbkp_sel   TO t_rbkp_sel.

        if wl_zlest0221_out-nfps is not INITIAL.

      SEARCH wl_zlest0221_out-nfps FOR '-' AND MARK.
    if sy-subrc = 0.
    CONCATENATE  'xblnr  = ''' wl_zlest0221_out-nfps '''' INTO t_where-line .
    APPEND t_where.
    else.
      CONCATENATE  ' xblnr LIKE ''' wl_zlest0221_out-nfps '-%''' INTO t_where-line .
    APPEND t_where.
    endif.
    endif.

endif.

     IF  t_rbkp_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, lifnr, xblnr, stblg
      FROM rbkp
      INTO TABLE @DATA(t_rbkp)
       FOR ALL ENTRIES IN @t_rbkp_sel
     WHERE lifnr = @t_rbkp_sel-lifnr
      " AND xblnr = @t_rbkp_sel-xblnr
       AND stblg = @t_rbkp_sel-stblg
      and (t_where).
  ENDIF.

  IF t_rbkp[] IS NOT INITIAL.
    SELECT belnr, gjahr, ebeln, matnr
      FROM ekbe
      INTO TABLE @DATA(t_ekbe)
       FOR ALL ENTRIES IN @t_rbkp
     WHERE belnr = @t_rbkp-belnr
       AND gjahr = @t_rbkp-gjahr.
  ENDIF.

    IF t_ekbe[] IS NOT INITIAL.
    SELECT matnr, maktx
      FROM makt
      INTO TABLE @DATA(t_makt)
       FOR ALL ENTRIES IN @t_ekbe
     WHERE matnr = @t_ekbe-matnr
       AND spras = @sy-langu.
  ENDIF.

  LOOP AT t_rbkp     INTO DATA(w_rbkp).
    w_bkpf_sel-bukrs    = w_rbkp-bukrs.
    w_bkpf_sel-gjahr    = w_rbkp-gjahr.
    w_bkpf_sel-awkey    = w_rbkp-belnr && w_rbkp-gjahr.
    APPEND w_bkpf_sel  TO t_bkpf_sel.
  ENDLOOP.

  IF t_bkpf_sel[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr, awkey
      FROM bkpf
      INTO TABLE @DATA(t_bkpf)
       FOR ALL ENTRIES IN @t_bkpf_sel
     WHERE bukrs = @t_bkpf_sel-bukrs
       AND gjahr = @t_bkpf_sel-gjahr
       AND awkey = @t_bkpf_sel-awkey.
  ENDIF.

  IF t_bkpf[] IS NOT INITIAL.
    SELECT bukrs, belnr, gjahr
      FROM bsik
      INTO TABLE @DATA(t_bsik)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.

    SELECT bukrs, belnr, gjahr, augbl, augdt
      FROM bsak
      INTO TABLE @DATA(t_bsak)
       FOR ALL ENTRIES IN @t_bkpf
     WHERE bukrs = @t_bkpf-bukrs
       AND belnr = @t_bkpf-belnr
       AND gjahr = @t_bkpf-gjahr.
  ENDIF.


  READ TABLE t_lfa1 INTO w_lfa1 WITH KEY stcd1 = wl_ZLEST0221_out-cnpj_transbordo
                                  BINARY SEARCH.

    READ TABLE t_rbkp INTO w_rbkp WITH KEY lifnr = w_lfa1-lifnr
                                           xblnr = wl_ZLEST0221_out-nfps
                                           stblg = abap_off.

    READ TABLE t_ekbe INTO DATA(w_ekbe) WITH KEY belnr = w_rbkp-belnr
                                                 gjahr = w_rbkp-gjahr.

    READ TABLE t_makt INTO DATA(w_makt) WITH KEY matnr = w_ekbe-matnr.

    l_awkey = w_rbkp-belnr && w_rbkp-gjahr.

    READ TABLE t_bkpf INTO DATA(w_bkpf) WITH KEY bukrs = w_rbkp-bukrs
                                                 gjahr = w_rbkp-gjahr
                                                 awkey = l_awkey.

    LOOP AT t_bsik INTO DATA(w_bsik) WHERE bukrs = w_bkpf-bukrs
                                       AND belnr = w_bkpf-belnr
                                       AND gjahr = w_bkpf-gjahr.
    ENDLOOP.



IF sy-subrc <> 0.
      LOOP AT t_bsak INTO DATA(w_bsak) WHERE bukrs = w_bkpf-bukrs
                                         AND belnr = w_bkpf-belnr
                                         AND gjahr = w_bkpf-gjahr.
        wl_ZLEST0221_out-augbl        = w_bsak-augbl.
        wl_ZLEST0221_out-augdt        = w_bsak-augdt.
        EXIT.
      ENDLOOP.
    ENDIF.


   wl_ZLEST0221_out-ebeln            = w_ekbe-ebeln.
    wl_ZLEST0221_out-belnr           = w_rbkp-belnr.
    wl_ZLEST0221_out-matnr           = w_ekbe-matnr.
    wl_ZLEST0221_out-matxt           = w_makt-maktx.
  ENDIF.


MOVE-CORRESPONDING wl_ZLEST0221_out TO p_saida.
clear: w_lfa1, w_rbkp, l_awkey, t_where[].
ENDFORM.

FORM f_exit_ZLEST0221_0005 CHANGING p_registro_manter TYPE any.

DATA: wa_ZLEST0221 TYPE  ZLEST0221.


  MOVE-CORRESPONDING p_registro_manter TO wa_ZLEST0221.


  MOVE-CORRESPONDING wa_ZLEST0221 TO p_registro_manter.



ENDFORM.

FORM  f_exit_ZLEST0221_0009 TABLES it_excl_toolbar
                           USING p_db_tab.

  IF p_db_tab = 'ZLEST0221'.
    APPEND 'Novo'    TO it_excl_toolbar.
  ENDIF.
ENDFORM.
