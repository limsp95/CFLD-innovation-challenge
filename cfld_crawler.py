import scrapy

class cfldSpider(scrapy.Spider):
    name = 'cfld'

    start_urls = [
        'https://www.brickz.my/transactions/residential/selangor/ampang/landed/?range=1982+Aug-'
    ]
    def parse(self, response):
        pages = response.css("div.ptd_table_toolbar div.pagination select.pagination_select option").extract()
        count = 0
        if not pages:
            return
        else:
            subareas_url = response.xpath("//table[@id='ptd_list_table']//tr//td[1]//@href").extract()

            for subarea_url in subareas_url:
                url = response.urljoin(subarea_url)
                yield scrapy.Request(url,callback=self.parse1)


            yield scrapy.Request(url,callback=self.parse1)

           # for page in pages: #pagination
           #     count = count + 1
           #     redirect_to = "https://www.brickz.my/transactions/residential/selangor/ampang/landed/page/%page/?range=1982+Aug-" % count
           #     yield scrapy.Request((redirect_to), callback=self.parse)
    def parse1(self, response):
        lands = response.xpath("//table[@id='ptd_list_detail_table']//tr")
        count = 1
        print 'inside parse1'
        for land in lands:
            sel = "//tr[%d]" % count
            print sel
            date = land.xpath(sel+"//td[1]").extract_first()
            address = land.xpath(sel+"//td[2]").extract_first()
            buildingType = land.xpath(sel+"//td[3]").extract_first()
            print count

            count = count + 1

            yield {
                'Date': date,
                'Address': address,
                'BuildingType': buildingType

            }
