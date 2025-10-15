*----------------------------------------------------------------------*
***INCLUDE LZNFE_INBOUNDO07.
*----------------------------------------------------------------------*

DATA: LC_ALTEROU_1603 TYPE C.

*&---------------------------------------------------------------------*
*&      Module  STATUS_1603  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1603 OUTPUT.

  SET PF-STATUS 'PF1603'.
  SET TITLEBAR 'TL1603'.

  IF LC_ALTEROU_1603 EQ ABAP_TRUE.

    IF ZIB_NFE_DIST_FRT-ID_AGENT_FRETE IS NOT INITIAL.
      TRY .
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO( I_PARCEIRO = ZIB_NFE_DIST_FRT-ID_AGENT_FRETE
            )->GET_NAME( IMPORTING E_NAME = ZIB_NFE_DIST_FRT-DS_AGENT_FRETE
            ).
        CATCH ZCX_PARCEIROS.
      ENDTRY.
    ENDIF.

    IF ZIB_NFE_DIST_FRT-ID_PROPRIETARIO IS NOT INITIAL.
      TRY .
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO( I_PARCEIRO = ZIB_NFE_DIST_FRT-ID_PROPRIETARIO
            )->GET_NAME( IMPORTING E_NAME = ZIB_NFE_DIST_FRT-DS_PROPRIETARIO
            ).
        CATCH ZCX_PARCEIROS.
      ENDTRY.
    ENDIF.

    IF ZIB_NFE_DIST_FRT-ID_MOTORISTA IS NOT INITIAL.
      TRY .
          ZCL_FORNECEDORES=>ZIF_PARCEIROS~GET_INSTANCE(
            )->SET_PARCEIRO( I_PARCEIRO = ZIB_NFE_DIST_FRT-ID_MOTORISTA
            )->GET_NAME( IMPORTING E_NAME = DATA(E_NAME)
            ).
          ZIB_NFE_DIST_FRT-DS_MOTORISTA = E_NAME.
        CATCH ZCX_PARCEIROS.
      ENDTRY.
    ENDIF.

  ENDIF.

  CLEAR: LC_ALTEROU_1603.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1603_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1603_EXIT INPUT.
  CLEAR: LC_ALTEROU_1603.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1603  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1603 INPUT.

  CHECK LC_ALTEROU_1603 EQ ABAP_FALSE.

  CASE OK_CODE.
    WHEN 'CONFIRMAR'.
      TRY .
          OBJ_NFE_INBOUND->SET_INFO_TRANSPORTE(
              I_CK_TRANS_NF_PROPRI = ZDE_NFE_DIST_ALV-CK_TRANS_NF_PROPRI
              I_ZIB_NFE_DIST_FRT = ZIB_NFE_DIST_FRT
          ).
          OBJ_NFE_INBOUND->ZIF_CADASTRO~GRAVAR_REGISTRO( ).
          LEAVE TO SCREEN 0.
        CATCH ZCX_CADASTRO INTO EX_CADASTRO.
          EX_CADASTRO->PUBLISHED_ERRO( EXPORTING I_MSGTY = 'S' I_MSGTY_DISPLAY = 'E' ).
      ENDTRY.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_1603  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_1603 INPUT.
  LC_ALTEROU_1603 = ABAP_TRUE.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  GET_AGENTE  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE GET_AGENTE INPUT.

  CHECK ZIB_NFE_DIST_FRT-DS_PLACA_TRATOR IS NOT INITIAL.

  TRY .
      DATA(LC_TRACAO) = ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE( )->SET_VEICULO( I_PLACA = ZIB_NFE_DIST_FRT-DS_PLACA_TRATOR ).
      DATA(LC_ACHOU)  = ABAP_TRUE.
    CATCH ZCX_VEICULOS.
      LC_ACHOU  = ABAP_FALSE.
  ENDTRY.

  CASE LC_ACHOU.
    WHEN ABAP_TRUE.

      TRY .
          LC_TRACAO->GET_CK_VEICULO_TRACAO(
            )->GET_VALIDA_PLACA(
            ).

          IF ZIB_NFE_DIST_FRT-ID_AGENT_FRETE IS INITIAL AND
             ZIB_NFE_DIST_FRT-ID_PROPRIETARIO IS INITIAL.
            ZIB_NFE_DIST_FRT-ID_AGENT_FRETE  = LC_TRACAO->AT_VEICULO-PROPRIETARIO.
            ZIB_NFE_DIST_FRT-ID_PROPRIETARIO = LC_TRACAO->AT_VEICULO-PROPRIETARIO.
          ENDIF.

          CLEAR: LC_TRACAO.

        CATCH ZCX_VEICULOS INTO DATA(EX_VEICULOS).
          CLEAR: LC_TRACAO.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

    WHEN ABAP_FALSE.

      TRY .
          ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE(
             )->GET_VALIDA_PLACA( I_PLACA = CONV #( ZIB_NFE_DIST_FRT-DS_PLACA_TRATOR )
             ).
        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_PLACA1  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VAL_PLACA1 INPUT.

  CHECK ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_1 IS NOT INITIAL.

  TRY .
      DATA(LC_REBOQUE1) = ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE( )->SET_VEICULO( I_PLACA = ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_1 ).
      LC_ACHOU  = ABAP_TRUE.
    CATCH ZCX_VEICULOS.
      LC_ACHOU  = ABAP_FALSE.
  ENDTRY.

  CASE LC_ACHOU.
    WHEN ABAP_TRUE.

      TRY .
          LC_REBOQUE1->GET_CK_VEICULO_REBOQUE(
            )->GET_VALIDA_PLACA(
            ).

          CLEAR: LC_REBOQUE1.

        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          CLEAR: LC_REBOQUE1.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

    WHEN ABAP_FALSE.

      TRY .
          ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE(
             )->GET_VALIDA_PLACA( I_PLACA = CONV #( ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_1 )
             ).
        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_PLACA2  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VAL_PLACA2 INPUT.

  CHECK ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_2 IS NOT INITIAL.

  TRY .
      DATA(LC_REBOQUE2) = ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE( )->SET_VEICULO( I_PLACA = ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_2 ).
      LC_ACHOU  = ABAP_TRUE.
    CATCH ZCX_VEICULOS.
      LC_ACHOU  = ABAP_FALSE.
  ENDTRY.

  CASE LC_ACHOU.
    WHEN ABAP_TRUE.

      TRY .
          LC_REBOQUE2->GET_CK_VEICULO_REBOQUE(
            )->GET_VALIDA_PLACA(
            ).

          CLEAR: LC_REBOQUE2.

        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          CLEAR: LC_REBOQUE2.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

    WHEN ABAP_FALSE.

      TRY .
          ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE(
             )->GET_VALIDA_PLACA( I_PLACA = CONV #( ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_2 )
             ).
        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  VAL_PLACA3  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE VAL_PLACA3 INPUT.

  CHECK ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_3 IS NOT INITIAL.

  TRY .
      DATA(LC_REBOQUE3) = ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE( )->SET_VEICULO( I_PLACA = ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_3 ).
      LC_ACHOU  = ABAP_TRUE.
    CATCH ZCX_VEICULOS.
      LC_ACHOU  = ABAP_FALSE.
  ENDTRY.

  CASE LC_ACHOU.
    WHEN ABAP_TRUE.

      TRY .
          LC_REBOQUE3->GET_CK_VEICULO_REBOQUE(
            )->GET_VALIDA_PLACA(
            ).

          CLEAR: LC_REBOQUE3.

        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          CLEAR: LC_REBOQUE3.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

    WHEN ABAP_FALSE.

      TRY .
          ZCL_VEICULOS=>ZIF_VEICULOS~GET_INSTANCE(
             )->GET_VALIDA_PLACA( I_PLACA = CONV #( ZIB_NFE_DIST_FRT-DS_PLACA_REBOQ_3 )
             ).
        CATCH ZCX_VEICULOS INTO EX_VEICULOS.
          EX_VEICULOS->ZIF_ERROR~PUBLISHED_ERRO( EXPORTING I_MSGTY = 'E' ).
      ENDTRY.

  ENDCASE.

ENDMODULE.
