*&----------------------------------------------------------------------------*
*& CLASS SELECIONA_DADOS DEFINITION                                           *
*& AUTOR: ENIO JESUS                                                          *
*& 15.07.2015                                                                 *
*&----------------------------------------------------------------------------*

  PUBLIC SECTION.

*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0110                                       *
*&----------------------------------------------------------------------------*
    METHODS: Z_SELECIONA_DADOS_TELA_0110.
    METHODS: TBX_BUSCA_EQUIPM_DISPONIVEIS.
    METHODS: TBX_BUSCA_CENTRO_DISPONIVEIS.

*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0120                                       *
*&----------------------------------------------------------------------------*
    METHODS: Z_SELECIONA_DADOS_TELA_0120.
    METHODS: TBX_BUSCA_CENTRO_EMPRESTADOS.

    METHODS: TBX_BUSCA_EQUIPM_EMPRESTADOS.

*&----------------------------------------------------------------------------*
*& METHOD SELECIONA DADOS SUB-TELA 0130                                       *
*&----------------------------------------------------------------------------*
    METHODS  Z_SELECIONA_DADOS_TELA_0130.
    METHODS: TBX_BUSCA_CENTRO_RESPONSAVEL.

*&----------------------------------------------------------------------------*
*& METHOD Z_AUTHORITY_CHECK                                                   *
*&----------------------------------------------------------------------------*
    METHODS: Z_AUTHORITY_CHECK  IMPORTING
                                        OBJECT TYPE CHAR7
                                            ID TYPE CHAR5
                                         FIELD TYPE SWERK
                                        RETURN TYPE CHAR1.

*&----------------------------------------------------------------------------*
*& METHOD Z_STATUS_EQUIPAMENTO                                                *
*&----------------------------------------------------------------------------*
    METHODS: Z_STATUS_EQUIPAMENTO  IMPORTING
                                     EQUIPMENT TYPE EQUNR.

    METHODS: Z_DETALHES_EQUIPAMENTO IMPORTING
                                     EQUIPMENT TYPE EQUNR.

*&---------------------------------------------------------------------*
*& METHOD Z_CHECAR_EQUI_EMPRESTADO                                     *
*&---------------------------------------------------------------------*
    METHODS: Z_CHECAR_EQUI_EMPRESTADO IMPORTING
                                           MSG TYPE CHAR1
                                        RETURN TYPE CHAR1.

*&---------------------------------------------------------------------*
*& METHOD Z_ATUALIZA_TELA_POS_EMPRESTIMO                               *
*&---------------------------------------------------------------------*
    METHODS: Z_ATUALIZA_TELA_EMPRESTIMO.


  PRIVATE SECTION.
    DATA: WA_DATA_GENERAL         TYPE BAPI_ITOB,
          WA_DATA_SPECIFIC_EXP    TYPE BAPI_ITOB_EQ_ONLY,
          WA_RETURN               TYPE BAPIRET2,
          AT_RETURN               TYPE SY-SUBRC.
