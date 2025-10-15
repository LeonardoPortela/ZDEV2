*&--------------------------------------------------------------------&*
*&                        Desenvolvimento Interno                     &*
*&--------------------------------------------------------------------&*
*& Projeto..: AMaggi                                                  &*
*& Autor....: Nilton Marcelo Segantin                                 &*
*& Data.....: 10/10/2025                                              &*
*& Descrição: Serviço de Frete de Terceiros                           &*
*& Transação: ZLES0224 (Prest. Serv. Frete - Faturar)                 &*
*---------------------------------------------------------------------&*
*&                    Histórico de Modificações                       &*
*& Autor ABAP |Request    |Data       |Descrição                      &*
*&--------------------------------------------------------------------&*
*& NSEGATIN   |DEVK9A2T37 |10/10/2025 |Desenvolvimento Inicial.       &*
*&--------------------------------------------------------------------&*
REPORT zlesr0005.

*----------------------------------------------------------------------*
* I N C L U D E S                                                      *
*----------------------------------------------------------------------*
INCLUDE zlesr0005_top.
INCLUDE zlesr0005_scr.
INCLUDE zlesr0005_o01.
INCLUDE zlesr0005_i01.
INCLUDE zlesr0005_f01.

*----------------------------------------------------------------------*
* A T  S E L E C T I O N - S C R E E N                                 *
*----------------------------------------------------------------------*
AT SELECTION-SCREEN OUTPUT.

  CASE sy-dynnr.
    WHEN 0101.
      LOOP AT SCREEN.
        IF screen-required EQ 1     OR
           screen-group1   EQ 'OBL'.
          screen-required = 2.
          MODIFY SCREEN.

        ENDIF.

      ENDLOOP.

      IF sy-ucomm IS INITIAL.
* Restringe as opções da tela de seleção
        PERFORM zf_limit_select_option.

      ENDIF.

    WHEN OTHERS.
*   Do nothing
  ENDCASE.

AT SELECTION-SCREEN ON p_bukrs.
* Configuração e validação do respectivo campo acionado.
  PERFORM zf_setting_verify_field USING p_bukrs
                                        'P_BUKRS'
                                        '0101'.
