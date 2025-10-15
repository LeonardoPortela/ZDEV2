*&---------------------------------------------------------------------*
*& Report ZTESTE_FRETE
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zteste_frete.

TABLES: zsdt0001.

DATA: lc_fatura     TYPE REF TO zcl_faturamento_automatico,
      l_valor_frete TYPE kbetr_kond,
      l_moeda_frete TYPE konwa,
      w_zsdt0001    TYPE zde_les_zsdt0001,
      l_cockpit     TYPE zcockpit,
      t_saida       TYPE zde_les_saida_zsdt0001_t,
      w_saida       TYPE zde_les_saida_zsdt0001.


PARAMETERS: p_chref  TYPE zsdt0001-ch_referencia.

START-OF-SELECTION.

  CREATE OBJECT lc_fatura.

  SELECT SINGLE *
    FROM zsdt0001
    INTO @DATA(w_0001)
  WHERE ch_referencia = @p_chref.

  l_cockpit = '01'.

  PERFORM f_selecao_fat_autom IN PROGRAM zlesr0102    USING w_0001-ch_referencia
                                                            l_cockpit.
  PERFORM f_saida             IN PROGRAM zlesr0102.
  PERFORM f_recuperar_dados   IN PROGRAM zlesr0102 CHANGING t_saida
                                                            w_zsdt0001
                                                            w_saida.

  TRY .
      lc_fatura->get_valor_frete( EXPORTING i_zsdt0001    = w_zsdt0001
                                  IMPORTING e_valor_frete = l_valor_frete
                                            e_moeda_frete = l_moeda_frete ).
    CATCH zcx_error INTO DATA(ex_error).
  ENDTRY.
