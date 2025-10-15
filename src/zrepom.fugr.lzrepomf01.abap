*----------------------------------------------------------------------*
***INCLUDE LZREPOMF01.
*----------------------------------------------------------------------*

*&---------------------------------------------------------------------*
*&      Form  CADASTRO_ROTA
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_WA_ZLEST0121  text
*      -->P_IT_ZLEST0122_T  text
*----------------------------------------------------------------------*
FORM CADASTRO_ROTA.

  DATA: IT_ZLEST0122_T TYPE ZDE_ZLEST0122_T.

  CLEAR: WA_ZLEST0121, IT_ZLEST0122[], IT_ZLEST0122, WA_ZLEST0122, IT_ZLEST0122_T, IT_ZLEST0122_ALV[].

  CK_NOVA_ROTA = ABAP_FALSE.

  OBJ_ROTA->GET_REGISTRO( IMPORTING E_REGISTRO = WA_ZLEST0121 ).

  OBJ_ROTA->GET_PERCURSOS( IMPORTING E_PERCURSOS = IT_ZLEST0122_T ).

  IF WA_ZLEST0121-ID_ROTA IS INITIAL.
    CK_NOVA_ROTA = ABAP_TRUE.
  ENDIF.

  LOOP AT IT_ZLEST0122_T INTO WA_ZLEST0122.
    APPEND WA_ZLEST0122 TO IT_ZLEST0122.
  ENDLOOP.

  LOOP AT IT_ZLEST0122.
    CLEAR: IT_ZLEST0122_ALV.
    MOVE-CORRESPONDING IT_ZLEST0122 TO IT_ZLEST0122_ALV.
    APPEND IT_ZLEST0122_ALV.
  ENDLOOP.


  CLEAR: ZDE_ZLEST0121_ALV.

  MOVE-CORRESPONDING WA_ZLEST0121 TO ZDE_ZLEST0121_ALV.

  CALL SCREEN 0100 STARTING AT 04 02.

ENDFORM.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100_EXIT  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100_EXIT INPUT.
  LEAVE TO SCREEN 0.
ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.

  CHECK CK_ALTEROU_0100 EQ ABAP_FALSE.

  CASE OK_CODE.
    WHEN OK_GRAVAR.
      CLEAR OK_CODE.
      "CALL METHOD OBJ_ROTA->SET_ID_ROTA( EXPORTING I_ID_ROTA = ZLEST0121-ID_ROTA ).
      CALL METHOD OBJ_ROTA->SET_BUKRS( EXPORTING I_BUKRS = ZDE_ZLEST0121_ALV-BUKRS ).
      CALL METHOD OBJ_ROTA->SET_BRANCH( EXPORTING I_BRANCH = ZDE_ZLEST0121_ALV-BRANCH ).
      CALL METHOD OBJ_ROTA->SET_CD_PAIS( EXPORTING I_CD_PAIS = ZDE_ZLEST0121_ALV-CD_PAIS ).
      CALL METHOD OBJ_ROTA->SET_CD_CID_ORIGEM( EXPORTING I_CD_CID_ORIGEM = ZDE_ZLEST0121_ALV-CD_CID_ORIGEM ).
      CALL METHOD OBJ_ROTA->SET_CD_CID_DESTINO( EXPORTING I_CD_CID_DESTINO = ZDE_ZLEST0121_ALV-CD_CID_DESTINO ).
      CALL METHOD OBJ_ROTA->SET_TP_PROC_TRANSP( EXPORTING I_TP_PROC_TRANSP = ZDE_ZLEST0121_ALV-TP_PROC_TRANSP ).
      CALL METHOD OBJ_ROTA->SET_TP_IDA_VOLTA( EXPORTING I_TP_IDA_VOLTA = ZDE_ZLEST0121_ALV-TP_IDA_VOLTA ).
      CALL METHOD OBJ_ROTA->SET_DS_OBSERVACAO( EXPORTING I_DS_OBSERVACAO = ZDE_ZLEST0121_ALV-DS_OBSERVACAO ).
      CALL METHOD OBJ_ROTA->SET_TP_ST_ROTA( EXPORTING I_TP_ST_ROTA = ZDE_ZLEST0121_ALV-TP_ST_ROTA ).
      CALL METHOD OBJ_ROTA->SET_CK_ATIVO( EXPORTING I_CK_ATIVO = ZDE_ZLEST0121_ALV-CK_ATIVO ).

      IF OBJ_ROTA->GRAVAR( ) EQ ABAP_TRUE.
        CK_GRAVADO  = ABAP_TRUE.
        LEAVE TO SCREEN 0.
      ENDIF.
  ENDCASE.

ENDMODULE.

*&---------------------------------------------------------------------*
*&      Module  ALTEROU_ATRIBUTOS  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE ALTEROU_ATRIBUTOS INPUT.
  CK_ALTEROU_0100 = ABAP_TRUE.
ENDMODULE.
