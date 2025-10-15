FUNCTION zaa_sobra_fisica.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_GJAHR) TYPE  GJAHR OPTIONAL
*"  TABLES
*"      T_IMOBIL STRUCTURE  ANLA OPTIONAL
*"----------------------------------------------------------------------

  FREE: t_sobra,
        t_sobra_elim,
        t_dados.

* CHECK t_imobil[] IS NOT INITIAL.
  g_gjahr   = i_gjahr.
  t_dados[] = t_imobil[].

  READ TABLE t_dados INTO w_dados INDEX 1.
  l_bukrs = w_dados-bukrs.

*---------------------------------------------------
* selecao dados
*---------------------------------------------------
  PERFORM f_selecao_dados.

*---------------------------------------------------
*-Monta grid
*---------------------------------------------------
  PERFORM f_monta_dados USING 0.

*---------------------------------------------------
*-ALV
*---------------------------------------------------
  CALL SCREEN 0100 STARTING AT 08  05
                     ENDING AT 178 18.

ENDFUNCTION.
