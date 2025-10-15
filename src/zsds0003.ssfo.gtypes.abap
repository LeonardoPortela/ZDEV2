TYPES: BEGIN OF TY_HEADER,
        nro_sol_ov type zsdt0051-nro_sol_ov,
        ORG_VENDAS TYPE BEZEI20,
        EMISSOR    TYPE NAME1_GP,
        ENDERECO   TYPE STRAS_GP,
        MUNICIPIO  TYPE ORT01_GP,
        CNPJ       TYPE STCD1,
        CPF        TYPE STCD2,
        IE(15),
        UF         TYPE REGIO,
        CEP        TYPE PSTL2,
        CIDADE     TYPE ORT01,
        VKAUS      type zsdt0051-VKAUS,
        LOGISTICA  TYPE ZSDT0051-COMENT_LOGISTICA,
        data       type zsdt0051-data_atual,
       END OF TY_HEADER,

       BEGIN OF TY_COND_PGTO,
         FORMA_PGTO(23),
         CONDICAO(23),
         VENCIMENTO(10),
         FRETE TYPE ZSDT0051-INCO1,
         BANCO(50),
         AG(11),
         MOEDA(5),
         CONTA(18),
         DIG(2),
       END OF TY_COND_PGTO.

TYPES: TY_MARA TYPE TABLE OF MARA,
       TY_MAKT TYPE TABLE OF MAKT,
       ty_itens type table of zsdt0053,
       ty_lines type table of TLINE,
       TY_LOGISTICA TYPE TABLE OF ZSDT0055.

TYPES: TY_QTD TYPE ADGE_SOOWA,
       ty_tot_sc(14) type p decimals 2,
       ty_vlr_tot(14) type p decimals 2.







