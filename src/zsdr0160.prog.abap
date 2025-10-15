*--------------------------------------------------------------------*
*                         Consultoria                                *
*--------------------------------------------------------------------*
* Projeto..: AMAGGI                                                  *
* Autor....: Jaime Tassoni                                           *
* Data.....: 18.01.2022                                              *
* Descrição: Cockpit gerenciador transferencia estoque fardinmhos    *
* Report   : ZLESR0152                                               *
*--------------------------------------------------------------------*
* Projeto  : CS2023000189
*--------------------------------------------------------------------*

**********************************************************************
* INCLUDES
**********************************************************************
INCLUDE zsdr0160_top.
INCLUDE zsdr0160_screen.
INCLUDE zsdr0160_class.
INCLUDE zsdr0160_forms.

**********************************************************************
* START-OF-SELECTION
**********************************************************************
START-OF-SELECTION.

  CASE abap_true.
    WHEN p_painel.
      PERFORM f_selecao_dados.

      IF t_zsdt0330[] IS INITIAL.
        MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
        STOP.
      ENDIF.

      PERFORM f_processa_dados.

    WHEN p_status.

      CASE abap_true.
        WHEN p_formbl.
          PERFORM f_selecao_fardos.

          IF t_zsdt0330[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_fardos.

        WHEN p_geralo.
          PERFORM f_selecao_lotes.

          IF t_zsdt0330[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_lotes.

*-----comentado #129705-20.12.2023-JT-inicio
        WHEN p_estorn.
          PERFORM f_selecao_estorno.

          IF t_zsdt0330[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_estorno.
*-----comentado #129705-20.12.2023-JT-fim

        WHEN p_reenv.
          PERFORM f_selecao_reenvio_ov.

          IF t_zsdt0213_int[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_reenvio_ov.

        WHEN p_notafi.
          PERFORM f_selecao_notas_fiscais.

          IF t_zsdt0330[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_notas_fiscais.

        "SD - Ganho Peso Automatico Algodao US #145369 - WPP
        WHEN p_ganper.

           PERFORM f_selecao_ganho_perda_peso.

          IF t_zsdt0344[] IS INITIAL.
            MESSAGE s024(sd) WITH TEXT-100 DISPLAY LIKE 'W'.
            STOP.
          ENDIF.
          PERFORM f_processa_ganho_perda.

        "SD - Ganho Peso Automatico Algodao US #145369 - WPP - Fim


      ENDCASE.
  ENDCASE.


  PERFORM f_alv_saida.

**********************************************************************
**********************************************************************
