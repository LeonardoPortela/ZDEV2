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
REPORT zlesr0152.

**********************************************************************
* INCLUDES
**********************************************************************
INCLUDE zlesr0152_top.
INCLUDE zlesr0152_class.
INCLUDE zlesr0152_screen.
INCLUDE zlesr0152_forms.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
** US - 92467 - Inicio - CBRAND
"START-OF-SELECTION.

*  IF p_nfterc = 'X'.
*    PERFORM f_selecao_dados.
*
*    IF t_jlin[] IS INITIAL.
*      MESSAGE s024(sd) WITH text-100 text-101 DISPLAY LIKE 'W'.
*      STOP.
*    ENDIF.
*
*    PERFORM f_processa_dados.
*    PERFORM f_alv_saida.
*  ELSE.
*    PERFORM f_selecao_dados_nf_propria.
*    PERFORM f_processa_dados.
*    PERFORM f_alv_saida.
*
*  ENDIF.
** US - 92467 - Fim - CBRAND


**********************************************************************
***********************************************************************
