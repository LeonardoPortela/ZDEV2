*&---------------------------------------------------------------------*
*& Report  ZLESI0003L
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlesi0003c.

TYPE-POOLS: zlesi.

DATA: vg_cnpj           TYPE c LENGTH 16,
      vg_posto          TYPE string,
      vg_transp         TYPE string.

DATA: ti_cockpit_lote   TYPE zles_cockpit_lote_t,
      ti_cockpit_lancto TYPE zles_cockpit_lancto_t,
      ti_cockpit_deltas TYPE zles_cockpit_delta_t,
      ti_cockpit_logs   TYPE TABLE OF zlest0008 INITIAL SIZE 0 WITH HEADER LINE,
      wa_cockpit_lote   TYPE zles_cockpit_lote,
      wa_cockpit_lancto TYPE zles_cockpit_lancto,
      wa_cockpit_logs   TYPE zlest0008.

DATA: peso_origem      TYPE brgew_ap,
      peso_confirmado  TYPE brgew_ap,
      peso_importado   TYPE brgew_ap,
      vlrorigem        TYPE kwert,
      vlrconfirmado    TYPE kwert,
      vlrdiferenca     TYPE kwert,
      vlrprogramado    TYPE kwert,
      tpeso_origem     TYPE brgew_ap,
      tpeso_confirmado TYPE brgew_ap,
      tpeso_importado  TYPE brgew_ap,
      tvlrorigem       TYPE kwert,
      tvlrconfirmado   TYPE kwert,
      tvlrdiferenca    TYPE kwert,
      tvlrprogramado   TYPE kwert.

IMPORT ti_cockpit_lote   FROM MEMORY ID 'ti_cockpit_lote'.
IMPORT ti_cockpit_lancto FROM MEMORY ID 'ti_cockpit_lancto'.
IMPORT ti_cockpit_deltas FROM MEMORY ID 'ti_cockpit_deltas'.
IMPORT ti_cockpit_logs   FROM MEMORY ID 'ti_cockpit_logs'.

READ TABLE ti_cockpit_lote INDEX 1 INTO wa_cockpit_lote.

NEW-PAGE LINE-SIZE 160 LINE-COUNT 999.

WRITE: /.

SORT ti_cockpit_lancto BY deschvid.

FORMAT COLOR COL_NORMAL INTENSIFIED OFF.

tvlrorigem       = 0.
tvlrconfirmado   = 0.
tvlrdiferenca    = 0.
tvlrprogramado   = 0.
tpeso_origem     = 0.
tpeso_confirmado = 0.
tpeso_importado  = 0.

LOOP AT ti_cockpit_lancto INTO wa_cockpit_lancto.

  AT NEW deschvid.
    WRITE: /.
    ULINE.
    WRITE: /001 wa_cockpit_lancto-deschvid.
    ULINE.
    WRITE: /001 'Nr.CTRC',
            010 'C.Frete',
            020 'Peso Origem'     RIGHT-JUSTIFIED,
            040 'Peso Confirmado' RIGHT-JUSTIFIED,
            060 'Peso Importado'  RIGHT-JUSTIFIED,
            080 'Vlr. Origem'     RIGHT-JUSTIFIED,
            100 'Vlr. Confirmado' RIGHT-JUSTIFIED,
            120 'Vlr. Diferença'  RIGHT-JUSTIFIED,
            140 'Vlr. Programado' RIGHT-JUSTIFIED.
    ULINE.
    vlrorigem       = 0.
    vlrconfirmado   = 0.
    vlrdiferenca    = 0.
    vlrprogramado   = 0.
    peso_origem     = 0.
    peso_confirmado = 0.
    peso_importado  = 0.
  ENDAT.

  WRITE: /001 wa_cockpit_lancto-conhec,
          010 wa_cockpit_lancto-ctafrete,
          020 wa_cockpit_lancto-peso_origem,
          040 wa_cockpit_lancto-peso_confirmado,
          060 wa_cockpit_lancto-peso_importado,
          080 wa_cockpit_lancto-vlrorigem,
          100 wa_cockpit_lancto-vlrconfirmado,
          120 wa_cockpit_lancto-vlrdiferenca,
          140 wa_cockpit_lancto-vlrprogramado.

  peso_origem     = peso_origem     + wa_cockpit_lancto-peso_origem.
  peso_confirmado = peso_confirmado + wa_cockpit_lancto-peso_confirmado.
  peso_importado  = peso_importado  + wa_cockpit_lancto-peso_importado.
  vlrorigem       = vlrorigem       + wa_cockpit_lancto-vlrorigem.
  vlrconfirmado   = vlrconfirmado   + wa_cockpit_lancto-vlrconfirmado.
  vlrdiferenca    = vlrdiferenca    + wa_cockpit_lancto-vlrdiferenca.
  vlrprogramado   = vlrprogramado   + wa_cockpit_lancto-vlrprogramado.

  AT END OF deschvid.
    ULINE.
    WRITE: /020 peso_origem    ,
            040 peso_confirmado,
            060 peso_importado ,
            080 vlrorigem      ,
            100 vlrconfirmado  ,
            120 vlrdiferenca   ,
            140 vlrprogramado  .
    ULINE.
    tpeso_origem     = tpeso_origem     + peso_origem.
    tpeso_confirmado = tpeso_confirmado + peso_confirmado.
    tpeso_importado  = tpeso_importado  + peso_importado.
    tvlrorigem       = tvlrorigem       + vlrorigem.
    tvlrconfirmado   = tvlrconfirmado   + vlrconfirmado.
    tvlrdiferenca    = tvlrdiferenca    + vlrdiferenca.
    tvlrprogramado   = tvlrprogramado   + vlrprogramado.
  ENDAT.

ENDLOOP.

IF NOT ti_cockpit_lancto[] IS INITIAL.
  WRITE: /.
  ULINE.
  WRITE: /01 'Total Históricos'.
  ULINE.
  WRITE: /020 'Peso Origem'     RIGHT-JUSTIFIED,
          040 'Peso Confirmado' RIGHT-JUSTIFIED,
          060 'Peso Importado'  RIGHT-JUSTIFIED,
          080 'Vlr. Origem'     RIGHT-JUSTIFIED,
          100 'Vlr. Confirmado' RIGHT-JUSTIFIED,
          120 'Vlr. Diferença'  RIGHT-JUSTIFIED,
          140 'Vlr. Programado' RIGHT-JUSTIFIED.
  ULINE.
  WRITE: /020 peso_origem    ,
          040 peso_confirmado,
          060 peso_importado ,
          080 vlrorigem      ,
          100 vlrconfirmado  ,
          120 vlrdiferenca   ,
          140 vlrprogramado  .
  ULINE.
ENDIF.

IF NOT ti_cockpit_logs[] IS INITIAL.

  WRITE: /.

  WRITE: /01 'Log de Processamento de Importação de Arquivo.'.
  ULINE.

  LOOP AT ti_cockpit_logs INTO wa_cockpit_logs.
    WRITE: /01 wa_cockpit_logs-cont,
            12 wa_cockpit_logs-msgv1.
  ENDLOOP.
  ULINE.

ENDIF.

TOP-OF-PAGE.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  WRITE: 01 'Lote: ' ,
         07 wa_cockpit_lote-lote.
  ULINE.
  CONCATENATE 'Posto:         ' wa_cockpit_lote-dscodposto 'CNPJ:' wa_cockpit_lote-cnpj_posto INTO vg_posto SEPARATED BY space.
  CONCATENATE 'Transportadora:' wa_cockpit_lote-dscodtrp   'CNPJ:' wa_cockpit_lote-cnpj_trp   INTO vg_transp SEPARATED BY space.
  WRITE: /01 vg_posto.
  WRITE: /01 vg_transp.
  ULINE.

END-OF-PAGE.

  FORMAT COLOR COL_HEADING INTENSIFIED ON.
  ULINE.
  WRITE: 'Grupo André Maggi'.
