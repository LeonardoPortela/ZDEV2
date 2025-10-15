*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 18.01.2022                                              *
* Descrição: Vinculação de NF-e Remessa por Conta e Ordem            *
* Report   : ZLESR0152                                               *
*--------------------------------------------------------------------*
* Projeto  : CS2021001045                                            *
*--------------------------------------------------------------------*
REPORT zmmr0042.

**********************************************************************
* INCLUDES
**********************************************************************
INCLUDE zmmr0042_top.
INCLUDE zmmr0042_class.
INCLUDE zmmr0042_screen.
INCLUDE zmmr0042_forms.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

  CASE abap_true.
    WHEN p_painel.
      PERFORM f_selecao_dados.

      IF t_zppt0030[] IS INITIAL.
        MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'W'.
        STOP.
      ENDIF.

      PERFORM f_processa_dados.

    WHEN p_reenvi.
      PERFORM f_selecao_reenvio.

      IF t_zppt0006[] IS INITIAL.
        MESSAGE s024(sd) WITH text-100 DISPLAY LIKE 'W'.
        STOP.
      ENDIF.

      PERFORM f_processa_reenvio.
  ENDCASE.


  PERFORM f_alv_saida.

**********************************************************************
**********************************************************************
