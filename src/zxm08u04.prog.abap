*&---------------------------------------------------------------------*
*&  Include           ZXM08U04
*&---------------------------------------------------------------------*

*-CS2025000249-27.05.2025-#175255-JT-inicio
*  SELECT SINGLE *
*    INTO @DATA(_rseg)
*    FROM rseg
*   WHERE lfbnr = @i_ydrseg-lfbnr
*     AND lfgja = @i_ydrseg-lfgja
*     AND lfpos = @i_ydrseg-lfpos.
*
*  CHECK sy-subrc <> 0.
*
*  SELECT SINGLE *
*    INTO @DATA(_mseg)
*    FROM mseg
*   WHERE mblnr = @i_ydrseg-lfbnr
*     AND mjahr = @i_ydrseg-lfgja
*     AND zeile = @i_ydrseg-lfpos.
*
*  CHECK sy-subrc = 0 AND _mseg-charg IS NOT INITIAL.
*
*  SELECT SINGLE chave_nfe
*    INTO @DATA(_chave_nfe)
*    FROM zib_nfe_dist_ter
*   WHERE mblnr = @i_ydrseg-lfbnr
*     AND mjahr = @i_ydrseg-lfgja.
*
*  CHECK sy-subrc = 0.
*
*  SELECT SINGLE cd_lote_item
*    INTO @DATA(_cd_lote_item)
*    FROM zib_nfe_dist_lot
*   WHERE chave_nfe = @_chave_nfe
*     AND mblnr     = @i_ydrseg-lfbnr
*     AND mjahr     = @i_ydrseg-lfgja
*     AND zeile     = @i_ydrseg-lfpos.
*
*  IF sy-subrc = 0.
*    i_ydrseg-remng = i_ydrseg-menge.
*    i_ydrseg-refwr = i_ydrseg-wrbtr.
*  ENDIF.
*-CS2025000249-27.05.2025-#175255-JT-fim

*DATA: wl_campo(25),
*      t_msg TYPE  mrm_errprot OCCURS 10 WITH HEADER LINE,
*      wa_data TYPE sy-datum.
*
*FIELD-SYMBOLS: <fs_tmsg>  TYPE STANDARD TABLE,
*
*               <fs_rbkp>  TYPE rbkp,
*               <fs_invfo> TYPE invfo.
*IF sy-tcode = 'MIRO'.
*  DO 2 TIMES.
*    CASE sy-index.
*      WHEN 1.
*        wl_campo = '(SAPLMRMF)tab_errprot[]'.
*        ASSIGN (wl_campo) TO <fs_tmsg>.
*        IF sy-subrc = 0.
*          t_msg[] = <fs_tmsg>.
*        ENDIF.
*      WHEN 2.
*        wl_campo = '(SAPLMR1M)RBKP'.
*        ASSIGN (wl_campo) TO <fs_rbkp>.
*    ENDCASE.
*
*
*  ENDDO.
*  SORT t_msg BY msgty msgid msgno.
*  READ TABLE t_msg WITH KEY msgty  = 'I'
*                            msgid  = 'M8'
*                            msgno  = '318'
*                            BINARY SEARCH.
*  IF sy-subrc = 0.
*    IF <fs_rbkp>-zlspr IS INITIAL.
*      MESSAGE e398(00) WITH 'Existem adiantamentos em aberto bloquear'
*                            'a Fatura para compensação'.
*    ENDIF.
*  ENDIF.
*
*  wl_campo = '(SAPLFDCB)INVFO'.
*  ASSIGN (wl_campo) TO <fs_invfo>.
*
*  wa_data = <fs_invfo>-zfbdt.
*  ADD 3 TO wa_data.
*
*  IF wa_data GT <fs_invfo>-netdt.
*    MESSAGE w398(00) WITH 'A fatura lançada está fora do prazo de pagamento'
*                          '72 horas, a mesma será bloqueada e somente poderá'
*                          'ser liberada pelo Dpto.Liquidação'.
*  ENDIF.
*
*
*ENDIF.
