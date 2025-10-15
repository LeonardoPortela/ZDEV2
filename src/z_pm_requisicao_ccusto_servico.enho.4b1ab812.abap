"Name: \FU:CO_ZF_FILL_EBAN_FROM_AFVGD\SE:END\EI
ENHANCEMENT 0 Z_PM_REQUISICAO_CCUSTO_SERVICO.

* include ZCOUPA_INTEGRA.

 data: MSG          TYPE STRING.
 data w_campo(40).
 FIELD-SYMBOLS: <fs_vornr> TYPE any.
 w_campo = '(SAPLCOIH)AFVGD-VORNR'.

  "Modificação solicitada pelo chamado CS2017000903
  "para possibilitar criar requisição de compra PM
  "com serviço s/ erro de C. Custo.

  IF sy-tcode eq 'IW21' OR
     sy-tcode eq 'IW22' OR
     sy-tcode eq 'IW31' OR
     sy-tcode eq 'IW32' OR
     sy-tcode eq 'IW34'.
    IF ebkn_exp-kostl is INITIAL.
      move caufvd-kostl to ebkn_exp-kostl.
    ENDIF.
  ENDIF.

  ASSIGN (w_campo) TO <fs_vornr>.
  if <fs_vornr> is ASSIGNED.
    if <fs_vornr> = AFVGD_EXP-VORNR.
      SELECT SINGLE *
        FROM EBAN
        INTO @DATA(_eban)
        WHERE BANFN = @eban_exp-banfn
        and   BNFPO = @eban_exp-bnfpo.

        IF _eban-status_coupa EQ 'S'.
            CONCATENATE 'Item ' eban_exp-bnfpo 'integrado com COUPA, não é possivél modificar.' INTO MSG SEPARATED BY SPACE.
            MESSAGE  MSG TYPE 'E'.
        ENDIF.
      ENDIF.
    ENDIF.

    FIELD-SYMBOLS:  <IX_ESLL> TYPE ANY.

    TYPES: BEGIN OF TY_IX_ESLL.
          INCLUDE STRUCTURE MSUPDAP.
    TYPES: SELKZ LIKE RM11P-SELKZ.
    TYPES: END OF TY_IX_ESLL.
    "
    data:  IT_IX_ESLL1    TYPE STANDARD TABLE OF TY_IX_ESLL,
           IT_IX_ESLL2    TYPE STANDARD TABLE OF TY_IX_ESLL,
           WA_IX_ESLL     TYPE TY_IX_ESLL,
           MSG01          TYPE STRING,
           VG_ATWRT       type AUSP-ATWRT.

    refresh : IT_IX_ESLL1, IT_IX_ESLL2.
    ASSIGN ('(SAPLMLSP)IX_ESLL[]') TO <IX_ESLL>.
    IF <IX_ESLL> IS ASSIGNED.
      IT_IX_ESLL1 = <IX_ESLL>.
      IF IT_IX_ESLL1[] is INITIAL.
         SELECT SINGLE *
           from esll
           into CORRESPONDING FIELDS OF WA_IX_ESLL
           WHERE packno = EBAN_EXP-PACKNO.
         IF sy-subrc = 0.
            SELECT *
              from esll
              INTO CORRESPONDING FIELDS OF TABLE IT_IX_ESLL1
              WHERE  packno = WA_IX_ESLL-sub_packno.
         ENDIF.
      else.
          IT_IX_ESLL2 = <IX_ESLL>.
          delete IT_IX_ESLL2 where packno ne EBAN_EXP-PACKNO.
          READ TABLE IT_IX_ESLL2 INTO WA_IX_ESLL INDEX 1.
          DELETE IT_IX_ESLL1 WHERE packno ne WA_IX_ESLL-SUB_PACKNO or SRVPOS is INITIAL.
      endif.
    ENDIF.



    IF IT_IX_ESLL1 IS NOT INITIAL.
      READ TABLE IT_IX_ESLL1 INTO WA_IX_ESLL INDEX 1.
      CALL FUNCTION 'CONVERSION_EXIT_ALPHA_OUTPUT'
      EXPORTING
        INPUT  = WA_IX_ESLL-SRVPOS
      IMPORTING
        OUTPUT = VG_ATWRT.
      select SINGLE *
        from ausp
        into @DATA(_AUSP)
        where KLART = '200'
        and   ATWRT = @VG_ATWRT.
      IF sy-subrc = 0.
         eban_exp-matnr = _AUSP-OBJEK+0(18).
         SELECT SINGLE *
           from MAKT
           into @DATA(_MAKT)
           where MATNR = @eban_exp-matnr
           and   SPRAS = @sy-langu.
         eban_exp-TXZ01 = _MAKT-MAKTX.
      else.
        CONCATENATE 'Servico ' VG_ATWRT 'não tem material correspondente, contacte o SUPRIMENTOS' INTO MSG01 SEPARATED BY SPACE.
        MESSAGE  MSG01 TYPE 'E'.
      ENDIF.
   ENDIF.
* ENDIF.



ENDENHANCEMENT.
