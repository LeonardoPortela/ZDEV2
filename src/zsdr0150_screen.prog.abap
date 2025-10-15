*&---------------------------------------------------------------------*
*&  Include           ZLESR0152_SCREEN
*&---------------------------------------------------------------------*
*&--------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2MMR |12/06/2025 |Adequações pra geração Contrato&*
*&                                    |Compra.                        &*
*&                                    |Chamado: 168919 2ª Parte.      &*
*&--------------------------------------------------------------------&*
*--------------------------------------------------------------------*
* tela seleção
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-136.
**<<<------"168919 - NMS - INI------>>>
*PARAMETERS : p_insumo RADIOBUTTON GROUP g3 USER-COMMAND us1,
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS p_insumo RADIOBUTTON GROUP g3 USER-COMMAND us1.
    SELECTION-SCREEN COMMENT 03(26) TEXT-003.
    SELECTION-SCREEN POSITION 30.
    SELECTION-SCREEN COMMENT 32(27) TEXT-007 MODIF ID tip.
    SELECTION-SCREEN POSITION 60.
    PARAMETERS rb_venda RADIOBUTTON GROUP g5 USER-COMMAND us2 MODIF ID tip DEFAULT 'X'.
    SELECTION-SCREEN  COMMENT 62(10) TEXT-006 MODIF ID tip.
    SELECTION-SCREEN POSITION 73.
    PARAMETERS rb_compr RADIOBUTTON GROUP g5 MODIF ID tip.
    SELECTION-SCREEN COMMENT 75(10) TEXT-005 MODIF ID tip.

  SELECTION-SCREEN END   OF LINE.
**<<<------"168919 - NMS - FIM------>>>
  PARAMETERS : p_merint RADIOBUTTON GROUP g3.
SELECTION-SCREEN END   OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE TEXT-137.
  PARAMETERS : p_sintet RADIOBUTTON GROUP g4 MODIF ID c02 USER-COMMAND us2,   "<<<------"168919 - NMS ------>>>
               p_analit RADIOBUTTON GROUP g4 MODIF ID c02.                    "<<<------"168919 - NMS ------>>>
SELECTION-SCREEN END   OF BLOCK b2.

SELECTION-SCREEN BEGIN OF BLOCK b3 WITH FRAME TITLE TEXT-135.
  PARAMETERS : p_contra RADIOBUTTON GROUP g2 MODIF ID m1,
               p_venda  RADIOBUTTON GROUP g2 MODIF ID m1,
               p_distra RADIOBUTTON GROUP g2 MODIF ID m1,
               p_aditiv RADIOBUTTON GROUP g2 MODIF ID m1,
               p_decrec RADIOBUTTON GROUP g2 MODIF ID m1,
               p_doctod RADIOBUTTON GROUP g2 MODIF ID m1.
SELECTION-SCREEN END   OF BLOCK b3.

SELECTION-SCREEN BEGIN OF BLOCK b4 WITH FRAME TITLE TEXT-134.
  PARAMETERS : p_pend   RADIOBUTTON GROUP g1,
               p_conclu RADIOBUTTON GROUP g1,
               p_todos  RADIOBUTTON GROUP g1.
SELECTION-SCREEN END   OF BLOCK b4.

SELECTION-SCREEN BEGIN OF BLOCK b5 WITH FRAME TITLE TEXT-002.
  SELECT-OPTIONS:  s_vkorg    FOR zsdt0040-vkorg         MODIF ID vnd,                "<<<------"168919 - NMS ------>>>
                   s_vkbur    FOR zsdt0040-vkbur         MODIF ID vnd NO INTERVALS,   "<<<------"168919 - NMS ------>>>
                   s_docsi    FOR zsdt0040-doc_simulacao MODIF ID vnd,                "<<<------"168919 - NMS ------>>>
                   s_kunnr    FOR zsdt0040-kunnr         MODIF ID vnd,                "<<<------"168919 - NMS ------>>>
                   s_erdat    FOR zsdt0040-erdat         MODIF ID vnd NO-EXTENSION,   "<<<------"168919 - NMS ------>>>
                   s_cultu    FOR zsdt0040-cultura       MODIF ID vnd NO INTERVALS,   "<<<------"168919 - NMS ------>>>
                   s_safra    FOR zsdt0040-safra         MODIF ID vnd NO INTERVALS,   "<<<------"168919 - NMS ------>>>
                   s_spart    FOR zsdt0040-spart         MODIF ID vnd,                "<<<------"168919 - NMS ------>>>
                   s_moeda    FOR zsdt0040-waerk         MODIF ID vnd NO INTERVALS.   "<<<------"168919 - NMS ------>>>
**<<<------"168919 - NMS - INI------>>>
* Filtro da opção Compras.
  SELECT-OPTIONS:  s_bukrs FOR t001-bukrs          MODIF ID cmp NO INTERVALS,
                   s_werks FOR zmmt0035-werks      MODIF ID cmp,
                   s_nosol FOR zmmt0035-nro_sol_cp MODIF ID cmp,
                   s_ebeln FOR zmmt0035-ebeln      MODIF ID cmp,
                   s_lifnr FOR zmmt0035-lifnr      MODIF ID cmp,
                   s_safr2 FOR zmmt0035-safra      MODIF ID cmp NO INTERVALS,
                   s_dtatl FOR zmmt0035-data_atual MODIF ID cmp NO-EXTENSION,
                   s_cult2 FOR zmmt0035-cultura    MODIF ID cmp.
**<<<------"168919 - NMS - FIM------>>>
SELECTION-SCREEN END   OF BLOCK b5.
*SELECTION-SCREEN:  BEGIN OF SCREEN 0211 AS SUBSCREEN.
*PARAMETER: p_sim RADIOBUTTON GROUP rd1 USER-COMMAND uc_s,
*           p_nao RADIOBUTTON GROUP rd1.
*SELECTION-SCREEN SKIP.
*PARAMETER: p_vlr AS CHECKBOX USER-COMMAND cb_v.
*SELECTION-SCREEN:  END OF SCREEN 0211.

**********************************************************************
* inicio
**********************************************************************
INITIALIZATION.
  p_insumo = abap_true.
  p_sintet = abap_true.
  p_doctod = abap_true.
**<<<------"168919 - NMS - INI------>>>
* Restringe as opções da tela de seleção.
  PERFORM zf_limit_select_option.
**<<<------"168919 - NMS - FIM------>>>
*----------------------------------------------------------------------*
* SELECTION-SCREEN
*----------------------------------------------------------------------*
AT SELECTION-SCREEN.
**<<<------"168919 - NMS - INI------>>>
*  IF s_vkorg[] IS INITIAL OR
*     s_vkbur[] IS INITIAL.
*    MESSAGE s024(sd) WITH TEXT-300 DISPLAY LIKE 'E'.
*    STOP.
*  ENDIF.
  CHECK sy-ucomm EQ 'ONLI'.
  CASE abap_on.
    WHEN p_insumo.
      gr_bukrs[] = s_vkorg[].

    WHEN p_merint.
      gr_bukrs[] = s_bukrs[].

    WHEN OTHERS.
*   Do nothing
  ENDCASE.
* Dados para verificar permissão para executa a opção da tela.
  PERFORM zf_data_authority_check TABLES gr_bukrs
                                         s_vkbur
                                         s_werks
                                   USING p_insumo
                                         p_merint
                                         rb_venda
                                         rb_compr.

  CHECK NOT rb_venda IS INITIAL OR
        NOT p_merint IS INITIAL.
**<<<------"168919 - NMS - FIM------>>>
  LOOP AT s_vkbur.
    SELECT SINGLE vkbur
      INTO @DATA(w_0060)
      FROM zsdt0060
     WHERE usnam    = @sy-uname
       AND vkbur    = @s_vkbur-low
       AND programa = 'ZSDR016'.

    IF sy-subrc <> 0.
      MESSAGE s024(sd) WITH TEXT-301 s_vkbur-low DISPLAY LIKE 'E'.
      STOP.
    ENDIF.
  ENDLOOP.

*----------------------------------------------------------------------*
* SELECTION-SCREEN OUTPUT
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  LOOP AT SCREEN.
**<<<------"168919 - NMS - INI------>>>
*    IF screen-name = 'S_VKORG-LOW' OR
*       screen-name = 'S_VKBUR-LOW'.
    IF ( ( p_insumo      EQ abap_on   AND
           rb_venda      EQ abap_on ) OR
           p_merint      EQ abap_on   AND
         ( screen-group1 EQ 'CMP'     OR
           screen-group1 EQ 'C02' ) ).

      IF screen-group1 EQ 'CMP'.
        screen-input     = 0.
        screen-invisible = 1.
        MODIFY SCREEN.

      ENDIF.

    ENDIF.

    IF   p_insumo      EQ abap_on AND
         rb_compr      EQ abap_on AND
       ( screen-group1 EQ 'VND'   OR
         screen-group1 EQ 'C02' ).
      p_sintet     = abap_off.
      p_analit     = abap_on.
      screen-input = 0.

      IF screen-group1 EQ 'VND'.
        screen-invisible = 1.

      ENDIF.

      MODIFY SCREEN.

    ENDIF.

    IF     ( ( screen-name EQ 'S_VKORG-LOW'   OR
               screen-name EQ 'S_VKBUR-LOW' ) AND
             ( p_insumo    EQ abap_on         AND
               rb_venda    EQ abap_on ) )     OR
           ( ( screen-name EQ 'S_VKORG-LOW'   OR
               screen-name EQ 'S_VKBUR-LOW' ) AND
               p_merint    EQ abap_on )       OR
           (   screen-name EQ 'S_BUKRS-LOW'   AND
               p_insumo    EQ abap_on         AND
               rb_compr    EQ abap_on   ).
**<<<------"168919 - NMS - FIM------>>>
      screen-required = 2.
      MODIFY SCREEN.
**<<<------"168919 - NMS - INI------>>>
    ELSEIF   screen-name EQ 'S_VKORG-LOW' OR
             screen-name EQ 'S_VKBUR-LOW' OR
           ( screen-name EQ 'S_BUKRS-LOW' AND
             p_insumo    EQ abap_on       AND
             rb_compr    EQ abap_off   ).
      screen-required = 0.
      MODIFY SCREEN.

    ENDIF.
**<<<------"168919 - NMS - FIM------>>>
    IF screen-group1 = 'M1'.
      IF p_sintet = abap_true.
        screen-input  = 0.
        screen-invisible = 1.
      ELSE.
        screen-input  = 1.
        screen-invisible = 0.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
**<<<------"168919 - NMS - INI------>>>
    IF screen-group1 EQ 'TIP'.
      IF p_insumo EQ abap_off.
        screen-input     = 0.
        screen-invisible = 1.

      ELSE.
        screen-input     = 1.
        screen-invisible = 0.

      ENDIF.
      MODIFY SCREEN.

    ENDIF.
**<<<------"168919 - NMS - FIM------>>>
  ENDLOOP.
**<<<------"168919 - NMS - INI------>>>
*--------------------------------------------------------------------*
* AT SELECTION-SCREEN ON
*--------------------------------------------------------------------*
* O evento é executado quando passa pelo respectivo campo.
AT SELECTION-SCREEN ON s_vkorg.
* Configura  o e valida  o do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_vkorg-low
                                        'S_VKORG-LOW'
                                        '1000'.
* o evento é executado quando passa pelo respectivo campo.
AT SELECTION-SCREEN ON s_vkbur.
* Configura  o e valida  o do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_vkbur-low
                                        'S_VKBUR-LOW'
                                        '1000'.
* o evento é executado quando passa pelo respectivo campo.
AT SELECTION-SCREEN ON s_bukrs.
* Configura  o e valida  o do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING s_bukrs-low
                                        'S_BUKRS-LOW'
                                        '1000'.
**<<<------"168919 - NMS - FIM------>>>
