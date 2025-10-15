"Name: \PR:SAPMV45A\FO:USEREXIT_MOVE_FIELD_TO_VBAK\SE:BEGIN\EI
ENHANCEMENT 0 Z_USEREXIT_MOVE_FIELD_TO_VBAK.

* Inicio
*IF vbak-auart = 'YFTE'.
*   vbkd-kurrf = 'B'.
*ELSE.
*    vbkd-kurrf = 'M'.
*ENDIF.

* Vendas Frama - Inicio
DATA: tl_zsdt0021 TYPE TABLE OF zsdt0021,
      vl_lines    TYPE i.

DATA: wl_zsdt0053 TYPE zsdt0053,
      wl_zsdt0066 TYPE zsdt0066,
      wl_zsdt0100 TYPE zsdt0100,
      wl_setleaf  TYPE setleaf.

DATA: vl_ok TYPE c.
DATA: vl_tipo TYPE c LENGTH 2.

*  vbak-zzfield = xxxx-zzfield2.
CASE sy-tcode.
  WHEN: 'VA02'.

    CLEAR: vl_ok.
    SELECT SINGLE * FROM zsdt0053 INTO wl_zsdt0053 WHERE vbeln EQ vbak-vbeln.
    IF ( sy-subrc EQ 0 ).
      vl_ok = abap_true.
      vl_tipo = 'MI'.
    ELSE.
      SELECT SINGLE * FROM zsdt0100 INTO wl_zsdt0100 WHERE vbeln EQ vbak-vbeln.
      IF ( sy-subrc EQ 0 ).
        vl_ok = abap_true.
        vl_tipo = 'MI'.
      ENDIF.
    ENDIF.

    SELECT SINGLE * FROM zsdt0066 INTO wl_zsdt0066 WHERE vbeln EQ vbak-vbeln.
    IF ( sy-subrc EQ 0 ).
      vl_ok = abap_true.
      vl_tipo = 'LG'.
    ENDIF.

    SELECT COUNT(*) FROM zsdt0041 WHERE vbeln EQ vbak-vbeln.
    IF ( sy-subrc EQ 0 ).
      vl_ok = abap_true.
      vl_tipo = 'IN'.
    ELSE.
      SELECT COUNT(*) FROM zsdt0090  WHERE vbeln EQ vbak-vbeln AND estorno EQ abap_false.
      IF ( sy-subrc EQ 0 ).
        vl_ok = abap_true.
        vl_tipo = 'IN'.
      ENDIF.
    ENDIF.

    IF ( vl_ok EQ 'X' ) .

      CASE vl_tipo.
        WHEN 'MI'.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf
            WHERE setname EQ 'MAGGI_VA02_SOLOV'
              AND valfrom EQ sy-uname.

          IF ( sy-subrc EQ 0 ).

            IF vbak-auart  EQ 'ZFRM' AND NOT
               cvbak-vbeln IS INITIAL.
              SELECT *
                FROM zsdt0021
                INTO TABLE tl_zsdt0021
              WHERE  vbeln EQ cvbak-vbeln.
              DELETE tl_zsdt0021 WHERE: dta_preco   NE sy-datum   ,
                                        ordem_venda NE space      .
              IF tl_zsdt0021[] IS INITIAL.
                MESSAGE i836(sd) WITH 'Para Contrato de Referência,' ' ainda não foi Fixado Preço'.
                LEAVE TO CURRENT TRANSACTION.
              ENDIF.
              SORT tl_zsdt0021 BY id_preco ASCENDING.
              DELETE ADJACENT DUPLICATES FROM tl_zsdt0021 COMPARING id_preco.
              DESCRIBE TABLE tl_zsdt0021 LINES vl_lines.
              IF vl_lines GT 1.
                MESSAGE i836(sd) WITH 'Há mais de um Preço Fixado,' ' não é Possível Prosseguir.'.
                LEAVE TO CURRENT TRANSACTION.
              ENDIF.
            ENDIF.

          ELSE.
            MESSAGE i836(sd) WITH 'Essa Ordem só podem ser editada,' ' pela transação ZSDT0062.'.
            LEAVE TO CURRENT TRANSACTION.
          ENDIF.

        WHEN 'LG'.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf
            WHERE setname EQ 'MAGGI_VA02_SOLOV_LG'
              AND valfrom EQ sy-uname.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE i836(sd) WITH 'Essa Ordem só podem ser editada,' ' pela transação ZSDT0062.'.
            LEAVE TO CURRENT TRANSACTION.
          ENDIF.

        WHEN 'IN'.

          SELECT SINGLE * FROM setleaf INTO wl_setleaf
            WHERE setname EQ 'MAGGI_VA02_INSUMOS'
              AND valfrom EQ sy-uname.

          IF sy-subrc IS NOT INITIAL.
            MESSAGE i836(sd) WITH 'Essa Ordem só podem ser editada,' ' pela transação ZSDT0087.'.
            LEAVE TO CURRENT TRANSACTION.
          ENDIF.

      ENDCASE.
    ELSE.

      IF vbak-auart  EQ 'ZFRM' AND NOT
                cvbak-vbeln IS INITIAL.
        SELECT *
          FROM zsdt0021
          INTO TABLE tl_zsdt0021
        WHERE  vbeln EQ cvbak-vbeln.
        DELETE tl_zsdt0021 WHERE: dta_preco   NE sy-datum   ,
                                  ordem_venda NE space      .
        IF tl_zsdt0021[] IS INITIAL.
          MESSAGE i836(sd) WITH 'Para Contrato de Referência,' ' ainda não foi Fixado Preço'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
        SORT tl_zsdt0021 BY id_preco ASCENDING.
        DELETE ADJACENT DUPLICATES FROM tl_zsdt0021 COMPARING id_preco.
        DESCRIBE TABLE tl_zsdt0021 LINES vl_lines.
        IF vl_lines GT 1.
          MESSAGE i836(sd) WITH 'Há mais de um Preço Fixado,' ' não é Possível Prosseguir.'.
          LEAVE TO CURRENT TRANSACTION.
        ENDIF.
      ENDIF.

    ENDIF.

  WHEN: 'VA01'.

    IF vbak-auart  EQ 'ZFRM' AND NOT
        cvbak-vbeln IS INITIAL.
      SELECT *
        FROM zsdt0021
        INTO TABLE tl_zsdt0021
      WHERE  vbeln EQ cvbak-vbeln.
      DELETE tl_zsdt0021 WHERE: dta_preco   NE sy-datum   ,
                                ordem_venda NE space      .
      IF tl_zsdt0021[] IS INITIAL.
        MESSAGE i836(sd) WITH 'Para Contrato de Referência,' ' ainda não foi Fixado Preço'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
      SORT tl_zsdt0021 BY id_preco ASCENDING.
      DELETE ADJACENT DUPLICATES FROM tl_zsdt0021 COMPARING id_preco.
      DESCRIBE TABLE tl_zsdt0021 LINES vl_lines.
      IF vl_lines GT 1.
        MESSAGE i836(sd) WITH 'Há mais de um Preço Fixado,' ' não é Possível Prosseguir.'.
        LEAVE TO CURRENT TRANSACTION.
      ENDIF.
    ENDIF.

ENDCASE.

* Vendas Frama - Fim

ENDENHANCEMENT.
