*&---------------------------------------------------------------------*
*&  Include           ZPMR0009_PBI
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_1000  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE USER_COMMAND_1000 INPUT.
  CASE SY-UCOMM.
    WHEN 'EXIT'.
      LEAVE PROGRAM.
    WHEN 'FECHAR'.
      LEAVE TO SCREEN 1000.
    WHEN 'BUSCAR' OR 'ENTER'.
      PERFORM F_BUSCAR_EQUIPAMENTO USING GW_TELA-EQUNR.
    WHEN 'ELIMINAR'.
      PERFORM F_TELA_ELIMINAR.
  ENDCASE.
ENDMODULE.                 " USER_COMMAND_1000  INPUT

*----------------------------------------------------------------------*
*  MODULE USER_COMMAND_2000 INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE USER_COMMAND_2000 INPUT.
  CASE SY-UCOMM.
    WHEN 'FECHAR' OR 'LEAVE'.
      LEAVE TO SCREEN 0.
    WHEN 'ELIMINAR'.
      PERFORM F_ELIMINAR_EQUIP USING GW_TL_ELIMINA-EQUNR.
  ENDCASE.
ENDMODULE.                    "USER_COMMAND_1000 INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_MODFICAR_TELA  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_MODFICAR_TELA INPUT.
** Carrega descrição causador
  CLEAR GW_QPCT.
  READ TABLE GT_QPCT_CAUS INTO GW_QPCT WITH KEY CODE = GW_TL_ELIMINA-CODE.
  GW_TL_ELIMINA-DESC_CAUSA = GW_QPCT-KURZTEXT.

** Carrega descrição eliminador
  CLEAR GW_QPCT.
  READ TABLE GT_QPCT_ELIM INTO GW_QPCT WITH KEY CODE = GW_TL_ELIMINA-CODE_ELIMINADOR.
  GW_TL_ELIMINA-DESC_ELIMINADOR = GW_QPCT-KURZTEXT.

** Carrega descrição motivo
  CLEAR GW_QPCT.
  READ TABLE GT_QPCT_MOT INTO GW_QPCT WITH KEY CODE = GW_TL_ELIMINA-CODE_MOTIVO.
  GW_TL_ELIMINA-DESC_MOTIVO = GW_QPCT-KURZTEXT.

ENDMODULE.                 " MO_MODFICAR_TELA  INPUT
