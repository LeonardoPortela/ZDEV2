**&---------------------------------------------------------------------*
**&  Include           ZXM02U04
**&---------------------------------------------------------------------*

* Início Inclusão ME52N - Renato Sabbado - 22/08/2008
  DATA: vc_ebeln LIKE eban-ebeln,
        vc_ebelp LIKE eban-ebelp,
        vc_loekz LIKE ekpo-loekz.

  DATA: BEGIN OF ti_eban OCCURS 0,
         ebeln LIKE eban-ebeln,
         ebelp LIKE eban-ebelp,
        END OF ti_eban.

* Busca Requisição da tela
  FIELD-SYMBOLS: <fs_banfn> TYPE ANY.

  CONSTANTS: c_banfn(28) VALUE '(SAPLMEGUI)MEPO_SELECT-BANFN'.

  IF sy-tcode EQ 'ME52N'.
    ASSIGN (c_banfn) TO <fs_banfn>.

* Seleciona pedidos da Requisição
    SELECT ebeln ebelp
      FROM eban
      INTO TABLE ti_eban
      WHERE banfn EQ <fs_banfn>
        AND ebeln NE ''.

* Percorre pedidos verificando código de eliminação

    LOOP AT ti_eban.

      SELECT SINGLE loekz
        INTO vc_loekz
        FROM ekpo
        WHERE ebeln EQ ti_eban-ebeln
          AND ebelp EQ ti_eban-ebelp.

       "USER STORY 163016 - MMSILVA - 27.01.2025 - Inicio
*      IF vc_loekz EQ ''.
*        MESSAGE ID '00' TYPE 'E' NUMBER '398'
*          WITH 'Requisição não pode mais ser modificada.'
*               'Já existe pedido criado.'.
*        LEAVE.
*      ENDIF.
       "USER STORY 163016 - MMSILVA - 27.01.2025 - Fim
    ENDLOOP.

  ENDIF.

* Fim Inclusão ME52N - Renato Sabbado - 22/08/2008
