*&---------------------------------------------------------------------*
*&  Include           ZPMR0009_PBO
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_1000 OUTPUT.
  SET PF-STATUS '1000'.
  SET TITLEBAR '1000'.

ENDMODULE.                 " STATUS_1000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_2000 OUTPUT.
  SET PF-STATUS '2000'.
*  SET TITLEBAR 'xxx'.

ENDMODULE.                 " STATUS_2000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MO_CRIAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_CRIAR_TELA OUTPUT.
  DATA: LV_NUM_LINES TYPE N LENGTH 5.

  CLEAR: GW_LAYOUT.

** Cria campos ALV
  PERFORM F_MONTAR_ALV USING GV_TIPO.

  IF OBJ_CONTAINER IS NOT INITIAL.
    CALL METHOD OBJ_GRID->FREE.
    CALL METHOD OBJ_CONTAINER->FREE.

    FREE: OBJ_CONTAINER,
          OBJ_GRID.
  ENDIF.

** Cria container
  CREATE OBJECT OBJ_CONTAINER
    EXPORTING
      CONTAINER_NAME = 'OBJ_TELA'.

** Cria ALV
  CREATE OBJECT OBJ_GRID
    EXPORTING
      I_PARENT = OBJ_CONTAINER.

  GW_LAYOUT-CWIDTH_OPT = 'X'.
  GW_LAYOUT-ZEBRA      = 'X'.
** Exibir texto da tela de avisos
  GW_LAYOUT-GRID_TITLE = GW_MSG_AVISO.

  CALL METHOD OBJ_GRID->SET_TABLE_FOR_FIRST_DISPLAY
    EXPORTING
*      IT_TOOLBAR_EXCLUDING = IT_FUNCTION
      IS_LAYOUT            = GW_LAYOUT
      I_SAVE               = 'X'
    CHANGING
      IT_FIELDCATALOG      = GT_FIELDCAT[]
      IT_OUTTAB            = <FS_OUTTAB>.

  LV_NUM_LINES = LINES( <FS_OUTTAB> ).
  CONCATENATE LV_NUM_LINES 'Encontrado(s)' INTO GV_NUM_REG SEPARATED BY SPACE.

  MODIFY SCREEN.

ENDMODULE.                 " MO_CRIAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MO_MODIFICAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_MODIFICAR_TELA OUTPUT.
  LOOP AT SCREEN.
    IF SCREEN-NAME = 'BTN_ELIMINAR'.
      IF GV_EQUIP_INAT IS INITIAL.
        SCREEN-INVISIBLE = '0'.
      ELSE.
        SCREEN-INVISIBLE = '1'.
      ENDIF.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDMODULE.                 " MO_MODIFICAR_TELA  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE STATUS_3000 OUTPUT.
  SET PF-STATUS '3000'.
*  SET TITLEBAR 'xxx'.

**  Carrega dados de causador
  PERFORM F_CARREGAR_CAUSADOR.

**  Carrega dados de motivo
  PERFORM F_CARREGAR_MOTIVO.

**  Carrega dados de eliminador
  PERFORM F_CARREGAR_ELIMINADOR.

**  Carrega dados de eliminador
  PERFORM F_CARREGAR_CENTRO.

**  Carrega dados de local
  PERFORM F_CARREGAR_LOCAL USING    GW_TELA-IWERK
                           CHANGING GW_TL_ELIMINA-TPLNR.

ENDMODULE.                 " STATUS_3000  OUTPUT
*&---------------------------------------------------------------------*
*&      Module  MO_BUSCA_CAUSADOR  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_BUSCA_CAUSADOR INPUT.

  REFRESH: GT_DSELC, GT_RETURN_TAB.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CODE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'QPCT-CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_QPCT_CAUS
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                 " MO_BUSCA_CAUSADOR  INPUT

*----------------------------------------------------------------------*
*  MODULE MO_BUSCA_MOTIVO INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MO_BUSCA_MOTIVO INPUT.

  REFRESH: GT_DSELC, GT_RETURN_TAB.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CODE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'QPCT-CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_QPCT_MOT
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                    "MO_BUSCA_MOTIVO INPUT

*----------------------------------------------------------------------*
*  MODULE MO_BUSCA_eliminador INPUT
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
MODULE MO_BUSCA_ELIMINADOR INPUT.

  REFRESH: GT_DSELC, GT_RETURN_TAB.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'CODE'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'QPCT-CODE'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_QPCT_ELIM
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                    "MO_BUSCA_eliminador INPUT
*&---------------------------------------------------------------------*
*&      Module  MO_BUSCA_CENTRO  INPUT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
MODULE MO_BUSCA_CENTRO INPUT.

  REFRESH: GT_DSELC, GT_RETURN_TAB.

  CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
    EXPORTING
      RETFIELD        = 'ARBPL'
      DYNPPROG        = SY-REPID
      DYNPNR          = SY-DYNNR
      DYNPROFIELD     = 'CRHD-ARBPL'
      VALUE_ORG       = 'S'
    TABLES
      VALUE_TAB       = GT_CRHD
      RETURN_TAB      = GT_RETURN_TAB
      DYNPFLD_MAPPING = GT_DSELC.
ENDMODULE.                 " MO_BUSCA_CENTRO  INPUT
