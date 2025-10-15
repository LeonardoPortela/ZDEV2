"Name: \PR:SAPLCODT\FO:LST_REC_DEL_CHECK\SE:END\EI
ENHANCEMENT 0 Z_VALIDA_ELIMINACAO_OP_PM.
* Regra para eliminação de itens na aba operações.
  DATA: BEGIN OF WL_EKPO,
        EREKZ TYPE EKPO-EREKZ,
        ELIKZ TYPE EKPO-ELIKZ,
        BNFPO TYPE EKPO-BNFPO,
        BANFN TYPE BANFN,
      END OF WL_EKPO,

      BEGIN OF WL_EBAN,
        BANFN TYPE EBAN-BANFN,
        EBELN TYPE EBAN-EBELN,
        FRGZU TYPE EBAN-FRGZU,
        BNFPO TYPE EBAN-BNFPO,
      END OF WL_EBAN.

  DATA: VMSG    TYPE BAPI_MSG.

  IF ( SY-TCODE EQ 'IW32' )
  AND ( SY-UCOMM EQ 'OPT1' )
  and ( afvgd-BANFN is not initial ).
    SELECT SINGLE BANFN EBELN FRGZU BNFPO
      FROM EBAN
      INTO CORRESPONDING FIELDS OF WL_EBAN
      WHERE BANFN = afvgd-BANFN
       AND  BNFPO = afvgd-BNFPO.

    CHECK SY-SUBRC IS INITIAL.

**  Valida situação da operação.
    IF WL_EBAN-FRGZU = 'X'.
      SELECT SINGLE EREKZ ELIKZ BNFPO BANFN
        FROM EKPO
        INTO CORRESPONDING FIELDS OF WL_EKPO
        WHERE BANFN = WL_EBAN-BANFN
         AND  BNFPO = WL_EBAN-BNFPO.

      IF SY-SUBRC IS INITIAL.
        CONCATENATE 'Existem requisição aprovada (' WL_EBAN-BANFN '), pedido (' WL_EBAN-EBELN ') para a operação ('WL_EBAN-BNFPO').' INTO VMSG.
        MESSAGE VMSG TYPE 'I'.
        subrc = 4.
        MESSAGE E398(00) WITH 'Não foi possível eliminar o item.'.
      ELSE.
        CONCATENATE 'Existem requisição aprovada (' WL_EBAN-BANFN ') para a operação ('WL_EBAN-BNFPO').' INTO VMSG.
        MESSAGE VMSG TYPE 'I'.
        subrc = 4.
        MESSAGE E398(00) WITH 'Não foi possível eliminar o item.'.
      ENDIF.
    ENDIF.

**  Checa se a operação tem componente vinculado e então válida a situação do mesmo.
      SELECT SINGLE BANFN EBELN FRGZU BNFPO
        FROM EBAN
        INTO CORRESPONDING FIELDS OF WL_EBAN
        WHERE BANFN = RESBD-BANFNR
         AND  BNFPO = resbd-BANFPO.

      CHECK SY-SUBRC IS INITIAL.

      IF WL_EBAN-FRGZU = 'X'.
        SELECT SINGLE EREKZ ELIKZ BNFPO BANFN
          FROM EKPO
          INTO CORRESPONDING FIELDS OF WL_EKPO
          WHERE BANFN = WL_EBAN-BANFN
           AND  BNFPO = WL_EBAN-BNFPO.

        IF SY-SUBRC IS INITIAL.
          CONCATENATE 'Existem requisição aprovada (' WL_EBAN-BANFN '), pedido (' WL_EBAN-EBELN ') para o "componente" ('WL_EBAN-BNFPO').' INTO VMSG.
          MESSAGE VMSG TYPE 'I'.
          subrc = 4.
          MESSAGE E398(00) WITH 'Não foi possível eliminar a operação.'.
        ELSE.
          CONCATENATE 'Existem requisição aprovada (' WL_EBAN-BANFN ') para o "componente" ('WL_EBAN-BNFPO').' INTO VMSG.
          MESSAGE VMSG TYPE 'I'.
          subrc = 4.
          MESSAGE E398(00) WITH 'Não foi possível eliminar a operação.'.
        ENDIF.
      ENDIF.
  ENDIF.
ENDENHANCEMENT.
