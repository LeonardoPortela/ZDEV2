*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 26.08.2024                                              *
* Descrição: TRatamento particao de lotes                            *
*--------------------------------------------------------------------*
* Projeto  : CS2024000522
*--------------------------------------------------------------------*

FORM f_tratar_particao_lotes TABLES xkomdlgn STRUCTURE komdlgn.

  DATA: t_lotes        TYPE zib_nfe_dist_lot_t,
        w_lotes        TYPE zib_nfe_dist_lot,
        t_lotes_aux    TYPE zib_nfe_dist_lot_t,
        w_xkomdlgn     TYPE komdlgn,
        lc_xkomdlgn    TYPE komdlgn,
        l_tabix        TYPE sy-tabix,
        lc_item_number TYPE bapiibdlvitem-itm_number,
        lc_chave       TYPE char16.

*---------------------------------------
*-- importar LOTES do metodo ZCL_FRETE_REMESSA_TRANS->SET_CRIAR_REMESSA
*---------------------------------------
  IMPORT t_lotes TO t_lotes[] FROM MEMORY ID 'PARTICAO_LOTES'.

  CHECK t_lotes[] IS NOT INITIAL.

  LOOP AT xkomdlgn INTO w_xkomdlgn.
    l_tabix           = sy-tabix.
    lc_chave          = w_xkomdlgn-vgbel && w_xkomdlgn-vgpos+1(5).

    t_lotes_aux[]     = t_lotes[].
    DELETE t_lotes_aux WHERE chave_nfe <> lc_chave.
    DESCRIBE TABLE t_lotes_aux LINES DATA(l_lines).

    IF l_lines = 1.
      SELECT SINGLE lgort
        FROM ekpo
        INTO w_xkomdlgn-lgort
       WHERE ebeln = w_xkomdlgn-vgbel
         AND ebelp = w_xkomdlgn-vgpos.

      READ TABLE t_lotes INTO w_lotes WITH KEY chave_nfe = lc_chave.
      w_xkomdlgn-charg      = w_lotes-charg.
      w_xkomdlgn-lichn      = w_lotes-charg.
      w_xkomdlgn-umcha      = w_lotes-charg.
      MODIFY xkomdlgn    FROM w_xkomdlgn INDEX l_tabix.
      DELETE t_lotes WHERE chave_nfe = lc_chave.
    ENDIF.
  ENDLOOP.

  LOOP AT xkomdlgn INTO w_xkomdlgn WHERE datum IS INITIAL.
    l_tabix        = sy-tabix.
    lc_chave       = w_xkomdlgn-vgbel && w_xkomdlgn-vgpos+1(5).
    lc_xkomdlgn    = w_xkomdlgn.
    lc_item_number = w_xkomdlgn-rfpos.

    SELECT SINGLE matnr, xchpf
      FROM marc
      INTO @DATA(w_marc)
     WHERE matnr = @w_xkomdlgn-matnr
       AND werks = @w_xkomdlgn-werks.

    CHECK w_marc-xchpf = abap_true.

    SELECT SINGLE lgort
      FROM ekpo
      INTO lc_xkomdlgn-lgort
     WHERE ebeln = w_xkomdlgn-vgbel
       AND ebelp = w_xkomdlgn-vgpos.

    LOOP AT t_lotes    INTO w_lotes WHERE chave_nfe = lc_chave.
      w_xkomdlgn-rfpos    = 0.
*     w_xkomdlgn-traty    = '0001'.
*     w_xkomdlgn-tragr    = '0001'.
*     w_xkomdlgn-ladgr    = '0003'.
*     w_xkomdlgn-mfrgr    = '00000001'.
*     w_xkomdlgn-kzbew    = 'B'.
      w_xkomdlgn-xchar    = abap_true.
      w_xkomdlgn-xchpf    = abap_true.
      w_xkomdlgn-uecha    = lc_item_number.
      w_xkomdlgn-uepos    = lc_item_number.
      w_xkomdlgn-lgort    = lc_xkomdlgn-lgort.
      w_xkomdlgn-charg    = w_lotes-charg.
      w_xkomdlgn-lichn    = w_lotes-charg.
      w_xkomdlgn-umcha    = w_lotes-charg.
      w_xkomdlgn-chspl    = abap_true.
      w_xkomdlgn-lfimg    = w_lotes-menge.
      w_xkomdlgn-lgmng    = w_lotes-menge.
      w_xkomdlgn-ntgew    = w_lotes-menge.
      w_xkomdlgn-brgew    = w_lotes-menge.
      w_xkomdlgn-kzazu    = abap_true.
      w_xkomdlgn-datum    = sy-datum.
      APPEND w_xkomdlgn  TO xkomdlgn.
    ENDLOOP.

    IF sy-subrc = 0.
      w_xkomdlgn          = lc_xkomdlgn.
      w_xkomdlgn-rfpos    = lc_item_number.
*     w_xkomdlgn-traty    = '0001'.
*     w_xkomdlgn-tragr    = '0001'.
*     w_xkomdlgn-ladgr    = '0003'.
*     w_xkomdlgn-mfrgr    = '00000001'.
*     w_xkomdlgn-kzbew    = 'B'.
      w_xkomdlgn-xchar    = abap_true.
      w_xkomdlgn-xchpf    = abap_true.
      w_xkomdlgn-uecha    = 0.
      w_xkomdlgn-uepos    = 0.
      w_xkomdlgn-lgort    = lc_xkomdlgn-lgort.
      w_xkomdlgn-chspl    = abap_true.
      w_xkomdlgn-kcmeng   = lc_xkomdlgn-lfimg.
      w_xkomdlgn-kcbrgew  = lc_xkomdlgn-brgew.
      w_xkomdlgn-kcntgew  = lc_xkomdlgn-ntgew.
      w_xkomdlgn-kcgewei  = lc_xkomdlgn-gewei.
      w_xkomdlgn-charg    = w_lotes-charg. "abap_false.
      w_xkomdlgn-lichn    = w_lotes-charg. "abap_false.
      w_xkomdlgn-umcha    = w_lotes-charg.
      w_xkomdlgn-lfimg    = 0.             "lc_xkomdlgn-lfimg.
      w_xkomdlgn-lgmng    = 0.             "lc_xkomdlgn-lgmng.
      w_xkomdlgn-ntgew    = 0.             "lc_xkomdlgn-ntgew.
      w_xkomdlgn-brgew    = 0.             "lc_xkomdlgn-brgew.
      w_xkomdlgn-gewei    = abap_off.
      w_xkomdlgn-kzazu    = abap_true.
      w_xkomdlgn-datum    = sy-datum.
      MODIFY xkomdlgn  FROM w_xkomdlgn INDEX l_tabix.
    ENDIF.
  ENDLOOP.

ENDFORM.

*--------------------------------------------------------------------*
*--------------------------------------------------------------------*
