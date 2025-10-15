*&---------------------------------------------------------------------*
*& Report  ZLESR0065
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT  zlesr0065.

*&---------------------------------------------------------------------*
*&      Form  X_GERAR_FECHAMENTO
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM x_gerar_fechamento  USING p_obj_key.

  DATA: tl_zib_contabil TYPE TABLE OF zib_contabil WITH HEADER LINE,
        tl_zib_erro     TYPE TABLE OF zib_contabil_err WITH HEADER LINE,
        tl_zpfe_lote    TYPE TABLE OF zpfe_lote WITH HEADER LINE,
        tl_lote_item    TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        tl_zpfe_lote_item    TYPE TABLE OF zpfe_lote_item WITH HEADER LINE,
        tl_lote         TYPE lxhme_range_c10_t,
        wl_lote         TYPE lxhme_range_c10,
        wl_obj_key      TYPE zib_contabil-obj_key.

  SELECT *
    FROM zib_contabil
    INTO TABLE tl_zib_contabil
     WHERE obj_key EQ p_obj_key
       AND rg_atualizado EQ 'S'.

  IF sy-subrc IS INITIAL.
    SELECT *
      FROM zpfe_lote
      INTO TABLE tl_zpfe_lote
       WHERE nm_lote EQ p_obj_key(10).

    SELECT *
      FROM zib_contabil_err
       INTO TABLE tl_zib_erro
        FOR ALL ENTRIES IN tl_zib_contabil
         WHERE obj_key EQ tl_zib_contabil-obj_key.

    IF sy-subrc IS NOT INITIAL.
      wl_obj_key = p_obj_key.
      CONDENSE wl_obj_key NO-GAPS.
      CLEAR: wl_lote.
      REFRESH: tl_lote.

      wl_lote-sign    = 'I'.
      wl_lote-option  = 'EQ'.
      wl_lote-low     = wl_obj_key(10).
      APPEND wl_lote TO tl_lote.



      CALL FUNCTION 'Z_PFE_PSQ_ITENS'
       EXPORTING
*        P_LOTE_ALV        =
*        P_PESQUISAR       = 'X'
         plote             = tl_lote[]
       TABLES
         p_itens           = tl_lote_item
       EXCEPTIONS
         sem_itens         = 1
         OTHERS            = 2.

      IF sy-subrc IS INITIAL.
        LOOP AT tl_lote_item.
          READ TABLE tl_zpfe_lote
           WITH KEY nm_lote = tl_lote_item-nm_lote.
          IF sy-subrc IS INITIAL.
            tl_lote_item-dt_baixa = tl_zpfe_lote-dt_posicao.
            tl_lote_item-ck_conferido = 'X'.

            MODIFY tl_lote_item.
            MODIFY zpfe_lote_item FROM tl_lote_item.
          ENDIF.
        ENDLOOP.

        LOOP AT tl_lote_item.

            CALL FUNCTION 'Z_PFE_GERA_CONTAB'
              EXPORTING
                p_nm_lote        = tl_lote_item-nm_lote
                p_nm_lote_item   = tl_lote_item-nm_lote_item
              TABLES
                it_lote_item     = tl_lote_item
              EXCEPTIONS
                sem_lote         = 1
                sem_lote_item    = 2
                gerando_contb    = 3
                concluido_contb  = 4
                gerado_contb     = 5
                conta_deb_cred   = 6
                sem_proprietario = 7
                nao_conferido    = 8
                error            = 9
                sem_data_baixa   = 10
                OTHERS           = 11.
            IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
            ENDIF.

        ENDLOOP.
      ENDIF.
    ENDIF.
  ENDIF.

ENDFORM.                    "x_gerar_fechamento
