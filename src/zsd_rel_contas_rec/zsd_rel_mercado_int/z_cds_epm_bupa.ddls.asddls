@AbapCatalog.sqlViewName: 'Z_SQL_EPM_BUPA'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@ClientHandling.type: #CLIENT_DEPENDENT
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'EPM Business Partner Extraction'
@Analytics:{
    dataCategory: #FACT,
    dataExtraction: {
        enabled: true,
        delta.changeDataCapture: {
            mapping:
            [{table: 'SNWD_SO',
              role: #MAIN,
              viewElement: ['SalesOrderGuid'],
              tableElement: ['node_key']
             },
             {table: 'SNWD_SO_I',
              role: #LEFT_OUTER_TO_ONE_JOIN,
              viewElement: ['ItemGuid'],
              tableElement: ['node_key']
             },
             {table: 'SNWD_PD',
              role: #LEFT_OUTER_TO_ONE_JOIN,
              viewElement: ['ProductGuid'],
              tableElement: ['node_key']
             },
             {table: 'SNWD_TEXTS',
              role: #LEFT_OUTER_TO_ONE_JOIN,
              viewElement: ['TextGuid'],
              tableElement: ['node_key']
             }
            ]
       }
    }
}
define view Z_CDS_EPM_BUPA
    as select from snwd_so as so
    left outer join snwd_so_i as item on so.node_key = item.parent_key
    left outer join snwd_pd as prod on item.product_guid = prod.node_key
    left outer join snwd_texts as text on prod.name_guid = text.parent_key and text.language = 'P'
{
    key item.node_key       as ItemGuid,
    so.node_key             as SalesOrderGuid,
    so.so_id                as SalesOrderId,
    so.created_at           as CreatedAt,
    so.changed_at           as ChangedAt,
    so.buyer_guid           as BuyerGuid,
    so.currency_code        as CurrencyCode,
    so.gross_amount         as GrossAmount,
    so.net_amount           as NetAmount,
    so.tax_amount           as TaxAmount,
    item.so_item_pos        as ItemPosition,
    prod.product_id         as ProductID,
    text.text               as ProductName,   
    prod.category           as ProductCategory,
    item.gross_amount       as ItemGrossAmount,
    item.net_amount         as ItemNetAmount,
    item.tax_amount         as ItemTaxAmount,
    prod.node_key           as ProductGuid,
    text.node_key           as TextGuid
}
