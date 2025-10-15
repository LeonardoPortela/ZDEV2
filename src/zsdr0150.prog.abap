*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 09.12.2022                                              *
* Descrição: Gerenciamento Completo-Fluxo e Historico                *
* Report   : ZSDR0150                                                *
*--------------------------------------------------------------------*
* Projeto  : CS2022000686
*--------------------------------------------------------------------*
*&-------------------------------------------------------------------&*
*&                    Histórico de Modificações                      &*
*& Autor ABAP |Request    |Data       |Descrição                     &*
*&-------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações p/ geração Contrato&*
*&                                    |Compra.                       &*
*&                                    |Chamado: 168919 2ª Parte.     &*
*&-------------------------------------------------------------------&*
**********************************************************************
* INCLUDES
**********************************************************************
INCLUDE zsdr0150_top.
INCLUDE zsdr0150_screen.
INCLUDE zsdr0150_form.     "<<<------"168919 - NMS ------>>>
**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.
**<<<------"168919 - NMS - INI------>>>
  DATA(vl_tpinsm) = COND char1( WHEN rb_venda = abap_on THEN sy-abcde+21(1)    "V - Venda
                                WHEN rb_compr = abap_on THEN sy-abcde+2(1) ).  "C - Compra

  IF NOT p_insumo IS INITIAL AND
     NOT rb_compr IS INITIAL.
    s_cultu[] = s_cult2[].
    DATA(r_safr2) = s_safr2[].
    CLEAR: s_safr2[], s_safr2.
    s_safr2[] = VALUE #( FOR el_safr2 IN r_safr2 WHERE ( low IS NOT INITIAL )
                                                       ( sign   = el_safr2-sign
                                                         option = 'CP'
                                                         low    = |*{ el_safr2-low }*|
                                                         high   = space
                                                       )
                        ).

  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
*---------------------------
* Executa cockpit
*---------------------------
  CALL FUNCTION 'ZSD_INSUMOS_COCKPIT'
    EXPORTING
      i_insumo = p_insumo
      i_tpinsm = vl_tpinsm   "<<<------"168919 - NMS ------>>>
      i_merint = p_merint
      i_sintet = p_sintet
      i_analit = p_analit
      i_contra = p_contra
      i_venda  = p_venda
      i_distra = p_distra
      i_aditiv = p_aditiv
      i_decrec = p_decrec
      i_doctod = p_doctod
      i_pend   = p_pend
      i_conclu = p_conclu
      i_todos  = p_todos
    TABLES
      i_vkorg  = s_vkorg
      i_vkbur  = s_vkbur
      i_docsi  = s_docsi
      i_kunnr  = s_kunnr
      i_erdat  = s_erdat
      i_cultu  = s_cultu
      i_safra  = s_safra
      i_spart  = s_spart
**<<<------"168919 - NMS - INI------>>>
*     i_moeda  = s_moeda.
      i_moeda  = s_moeda
      i_bukrs  = s_bukrs
      i_werks  = s_werks
      i_nosol  = s_nosol
      i_ebeln  = s_ebeln
      i_lifnr  = s_lifnr
      i_safr2  = s_safr2
      i_dtatl  = s_dtatl.

  IF NOT p_insumo IS INITIAL AND
     NOT rb_compr IS INITIAL.
    CLEAR: s_cultu[], s_cultu.
    s_safr2[] = r_safr2[].

  ENDIF.
**<<<------"168919 - NMS - FIM------>>>
**********************************************************************
**********************************************************************
