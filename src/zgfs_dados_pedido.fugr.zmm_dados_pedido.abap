FUNCTION zmm_dados_pedido.
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     REFERENCE(I_EBELN) TYPE  EKPO-EBELN OPTIONAL
*"     REFERENCE(I_EBELP) TYPE  EKPO-EBELP OPTIONAL
*"     REFERENCE(I_BSART) TYPE  EKKO-BSART OPTIONAL
*"     REFERENCE(I_VBELN) TYPE  LIPS-VBELN OPTIONAL
*"  TABLES
*"      T_DADOS_REMESSA TYPE  ZZSDT_DADOS_REMESSA OPTIONAL
*"----------------------------------------------------------------------

  FREE: gt_dados_pedidos, gt_dados_remessa,
        it_itens_alv,     it_lotes, it_lotes_c, it_lotes_alv_t, it_lotes_alv_t, it_lotes_c, it_lotes_c,
        it_carac_alv_u,   zib_nfe_dist_lot,     it_lotes_alv_u.

  CLEAR: vg_display.

  CREATE OBJECT lc_frete_remessa_trans.  "*-CS2024000522-18.07.2024-JT-#147087-inicio

  IF NOT i_ebeln IS INITIAL OR i_vbeln IS NOT INITIAL..
    IF NOT i_ebeln IS INITIAL.
      SELECT SINGLE *
        FROM ekko INTO @DATA(wl_ekko)
        WHERE ebeln = @i_ebeln
        AND bsart = @i_bsart.

      CHECK sy-subrc = 0.

      SELECT ebeln, ebelp, werks, lgort, menge, meins
         FROM ekpo
         INTO TABLE @DATA(gt_ekpo)
         WHERE ebeln = @i_ebeln.
    ELSE.
      SELECT *
        FROM lips
        INTO TABLE @DATA(it_lips)
        WHERE vbeln = @i_vbeln.

      CHECK sy-subrc = 0.

      SELECT p~ebeln, p~ebelp, p~werks, p~lgort, p~menge, p~meins
             FROM ekpo AS p
          INTO TABLE @gt_ekpo
          FOR ALL ENTRIES IN @it_lips
          WHERE ebeln = @it_lips-vgbel.

      CHECK sy-subrc = 0.

    ENDIF.



    SORT gt_ekpo BY ebeln ebelp.

    SELECT *
           FROM ekpa
           INTO TABLE @DATA(t_ekpa)
           FOR ALL ENTRIES IN @gt_ekpo
           WHERE ebeln = @gt_ekpo-ebeln
           AND   parvw  = 'PR'.

    SELECT *
           FROM eket
           INTO TABLE @DATA(gt_eket)
           FOR ALL ENTRIES IN @gt_ekpo
           WHERE ebeln = @gt_ekpo-ebeln
           AND  ebelp  = @gt_ekpo-ebelp.

    IF NOT i_ebeln IS INITIAL.
      SELECT vbeln, posnr, matnr, vgbel, vgpos, lfimg
         FROM lips
         INTO TABLE @DATA(gt_lips)
         FOR ALL ENTRIES IN @gt_ekpo
         WHERE vgbel = @gt_ekpo-ebeln.
    ELSE.
      SELECT vbeln, posnr, matnr, vgbel, vgpos, lfimg
        FROM lips
        INTO TABLE @gt_lips
        WHERE vbeln = @i_vbeln.
    ENDIF.


    SORT gt_eket BY ebeln ebelp.
    SORT gt_lips BY vgbel vgpos.

    LOOP AT gt_lips INTO DATA(wa_lips).

      READ TABLE gt_ekpo INTO DATA(wa_ekpo) WITH KEY ebeln = wa_lips-vgbel ebelp = wa_lips-vgpos BINARY SEARCH.
      IF sy-subrc EQ 0.
        wa_dados_pedidos-ebeln     = wa_ekpo-ebeln.
        wa_dados_pedidos-ebelp     = wa_ekpo-ebelp.
        wa_dados_pedidos-menge     = wa_lips-lfimg.
        wa_dados_pedidos-meins     = wa_ekpo-meins.
        wa_dados_pedidos-matnr     = wa_lips-matnr.
        wa_dados_pedidos-ped_werks = wa_ekpo-werks.
        wa_dados_pedidos-lgort     = wa_ekpo-lgort.

        "    Transformar XPED-RESWR é um código de fornecedor
        CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
          EXPORTING
            input  = wa_ekpo-werks
          IMPORTING
            output = wa_dados_pedidos-ped_entrega.


        READ TABLE gt_eket INTO DATA(wa_eket) WITH KEY ebeln = wa_ekpo-ebeln ebelp = wa_ekpo-ebelp BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_dados_pedidos-charg  = wa_eket-charg.
        ENDIF.

        READ TABLE t_ekpa INTO DATA(wa_ekpa) WITH KEY ebeln = wa_ekpo-ebeln BINARY SEARCH.
        IF sy-subrc EQ 0.
          wa_dados_pedidos-ped_coleta = wa_ekpa-lifn2.
        ENDIF.
        APPEND wa_dados_pedidos TO gt_dados_pedidos.
      ENDIF.
    ENDLOOP.
  ENDIF.

*-----------------------------
* Popup dados de pedido
*-----------------------------

  IF NOT gt_dados_pedidos[] IS INITIAL.
    vg_display = abap_true.
    CALL SCREEN 100  STARTING AT 034 04  "*-CS2024000522-18.07.2024-JT-#147087-inicio
                       ENDING AT 072 15.
    RETURN.                              "*-CS2024000522-18.07.2024-JT-#147087-inicio
  ELSE.
    CLEAR wa_dados_pedidos.
    APPEND wa_dados_pedidos TO gt_dados_pedidos.
    APPEND wa_dados_pedidos TO gt_dados_pedidos.
    APPEND wa_dados_pedidos TO gt_dados_pedidos.
    APPEND wa_dados_pedidos TO gt_dados_pedidos.
    APPEND wa_dados_pedidos TO gt_dados_pedidos.
  ENDIF.

*-CS2024000522-18.07.2024-JT-#147087-inicio
* CALL SCREEN 100  STARTING AT 034 04
*                    ENDING AT 072 15.
  CALL SCREEN 1600 STARTING AT  30 02
                     ENDING AT 162 20.
*-CS2024000522-18.07.2024-JT-#147087-fim

  t_dados_remessa[] = gt_dados_remessa[].
  vg_display = abap_false.

ENDFUNCTION.
