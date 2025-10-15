*&---------------------------------------------------------------------*
*& Report  ZLESR0102
*&
*&---------------------------------------------------------------------*
*&TITULO  : Cockipt de Faturamento Romaneio
*&AUTOR   :
*&DATA.   : 04.05.2017
*TRANSACAO:
*&---------------------------------------------------------------------*

INCLUDE zlesr0102_top.

*-#133089-21.02.2024-JT-inicio
*DATA: lc_faturamento_automatico TYPE REF TO zcl_faturamento_automatico,
*      vg_faturamento_autom      TYPE char01.
*-#133089-12.02.2024-JT-fim

INCLUDE zlesr0102_class.
INCLUDE zlesr0102_form.
INCLUDE zlesr0102_form2.  "*-CS2024000522-11.09.2024-JT-#151751
INCLUDE zlesr0102_pbo.
INCLUDE zlesr0102_pai.

FORM f_action_fat_externo
     USING p_saida        TYPE ty_saida
           p_tipo_chamada TYPE char01
  CHANGING it_saida_romaneios TYPE zde_les_saida_zsdt0001_t
           it_tab_bapiret1    TYPE tab_bapiret1
           lc_texto_erro      TYPE string.

  TRY .
      PERFORM f_action_user_fatura  USING p_saida p_tipo_chamada CHANGING it_saida_romaneios it_tab_bapiret1.
    CATCH zcx_carga INTO DATA(ex_carga).
      ex_carga->published_erro( EXPORTING  i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lc_texto_erro WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

ENDFORM.


FORM f_action_tra_externo
     USING p_saida        TYPE ty_saida
           p_tipo_chamada TYPE char01
  CHANGING it_saida_romaneios TYPE zde_les_saida_zsdt0001_t
           it_tab_bapiret1    TYPE tab_bapiret1
           lc_texto_erro      TYPE string.

  TRY .
      PERFORM f_action_user_transp USING p_saida p_tipo_chamada CHANGING it_saida_romaneios it_tab_bapiret1.
    CATCH zcx_carga INTO DATA(ex_carga).
      ex_carga->published_erro( EXPORTING  i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lc_texto_erro WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

ENDFORM.

FORM f_atual_frete_externo
     USING pa_zsdt0001    TYPE ty_zsdt0001
           p_tipo_chamada TYPE char01
  CHANGING p_saida       TYPE ty_saida
           lc_texto_erro TYPE string.
  TRY .
      PERFORM f_atual_frete USING pa_zsdt0001 p_tipo_chamada CHANGING p_saida.
    CATCH zcx_carga INTO DATA(ex_carga).
      ex_carga->published_erro( EXPORTING  i_msgty = 'S' i_msgty_display = 'S' ).
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno INTO lc_texto_erro WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDTRY.

ENDFORM.

******************************************************
******************************************************
