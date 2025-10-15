FUNCTION zfmm_bloqueio_recap .
*"----------------------------------------------------------------------
*"*"Interface local:
*"  IMPORTING
*"     VALUE(I_EKKO) TYPE  EKKO OPTIONAL
*"     VALUE(I_EKPO) LIKE  EKPO STRUCTURE  EKPO OPTIONAL
*"  EXPORTING
*"     VALUE(E_ZRECAP) TYPE  CHAR1
*"  TABLES
*"      TEKKN STRUCTURE  EKKNU OPTIONAL
*"----------------------------------------------------------------------
*----------------------------------------------------------------------*
*                            AMAGGI                                    *
*----------------------------------------------------------------------*
* Descrição  : Bloqueio para Compras com RECAP                         *
* Tipo       : EXIT                                                    *
* Projeto    :                                                         *
* Ampliação  :                                                         *
*----------------------------------------------------------------------*
* Histórico das modificações                                           *
*----------------------------------------------------------------------*
* Data    | Nome      | Request | Descrição                            *
*----------------------------------------------------------------------*
**13.05.20|JALEXANDRE |         | Desenvolvimento Inicial              *
*----------------------------------------------------------------------*
  DATA: t_return TYPE TABLE OF bapiret2 WITH HEADER LINE.

  DATA: w_allocations  TYPE bapi1022_feglg004,
        w_allocationsx TYPE bapi1022_feglg004x.

  DATA: l_companycode TYPE bapi1022_1-comp_code,
        l_asset       TYPE bapi1022_1-assetmaino,
        l_subnumber   TYPE bapi1022_1-assetsubno,
        l_answer(1).

  "Flag de verificação de Ativo\Inativo
  SELECT SINGLE * FROM tvarvc
    INTO @DATA(w_recap_inativo)
    WHERE name = 'BLOQUEIO_RECAP'
      AND low  = 'X'.

  IF i_ekpo-knttp = 'A' AND  sy-subrc IS NOT INITIAL.

    "Verificar se é Compra para Imobilizado
    SELECT SINGLE * FROM j_1btxic3
      INTO @DATA(w_j_1btxic3)
      WHERE land1	=	'BR'
        AND gruop	=	'42'
        AND value	=	@i_ekpo-j_1bnbm.

    IF sy-subrc IS INITIAL.

      DATA(t_ekkn) = tekkn[].
      DELETE t_ekkn WHERE ebeln <> i_ekpo-ebeln OR ebelp <> i_ekpo-ebelp.

    ENDIF.

    IF t_ekkn[] IS NOT INITIAL.

      "Buscar número do documento do lançamento de imobilizado para o item do pedido
      SELECT bukrs, anln1, anln2 FROM anla
        INTO TABLE @DATA(t_anla)
        FOR ALL ENTRIES IN @t_ekkn
        WHERE bukrs = @i_ekko-bukrs
          AND anln1 = @t_ekkn-anln1
          AND anln2 = @t_ekkn-anln2.

    ENDIF.

    IF t_anla[] IS NOT INITIAL.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = 'Confirmação'
          text_question         = 'Este produto pode ser adquirido com o benefício do RECAP. A compra está sendo realizada com tal benefício?'
          text_button_1         = 'Sim'(100)
          icon_button_1         = 'ICON_OKAY '
          text_button_2         = 'Não'(101)
          icon_button_2         = 'ICON_CANCEL'
          default_button        = '1'
          display_cancel_button = ' '
          start_column          = 25
          start_row             = 6
        IMPORTING
          answer                = l_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2.

      IF l_answer = 1.
        e_zrecap = 'S'. "SIM
      ELSE.
        e_zrecap = 'N'. "NÃO
      ENDIF.

    ENDIF.

  ENDIF.

ENDFUNCTION.
