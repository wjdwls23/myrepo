# 반도체 업종의 현황 분석

<br/>

주제 선정 이유 : 첫 번째로 반도체에 대한 도매인 지식을 쌓고 싶었고, 두 번째로 올해 반도체 1분기 기업성장과 수익 및 안정성이 악화되고 있다고 한다. 이는 매출액 상위 대기업의 대규모 영업손실이 전체 지표에 영향을 주었다는 기사를 보았다. 그래서 대기업을 제외한 나머지 반도체 관련 기업들의 영업이익증가율과 매출액증가율을 알아보고자 한다.

<br/>

## 1. 시가총액 상위 대기업인 삼성전자와 SK하이닉스의 매출액, 영업이익, 당기순이익 분석

<br/>

- 삼성전자 : 반도체 매출이 주요제품의 약 32.25% 차지
- SK하이닉스 : 반도체 매출 100%
- 두 기업 모두 주력 생산제품은 DRAM, 낸드플래쉬와 같은 메모리 반도체
<br/>
<img width="1155" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/ac86209c-c05c-405b-adf7-5873bc5301a1">
<br/>

```
- 2022년 1분기부터, 2023년 1분기까지 데이터이다.
- 작년에 비해 매출액과 영업이익이 감소하여 올해 1분기에 최저점을 찍었다.
  (※현재 반도체 불황 원인 : 코로나 19팬데믹때 IT관련 수요가 늘어나면서 반도체 수요가 폭발적으로 증가<br/>
  -> 그러나 엔데믹에 접어들면서 호황기에 과잉 설비투자했던 여파, 공급 과잉으로 인한 재고 부담, 반도체 가격 하락 등 악재로 큰 어려움 직면. 게다가 경기 침체와 인플레이션으로 인한 소비자들의 구매력 감소)
```

## 2. 나머지 반도체 관련 기업들의 분석

1) 매출액증가율, 영업이익증가율 분석

- 매출액 증가율(%) = {(당기 매출액-전기 매출액)/전기 매출액}*100
- 영업이익증가(%) = {(현 회계연도 영업이익-전 회계연도 영업이익)/전 회계연도 영업이익}*100<br/>
(영업이익 = 매출액-매출원가-판매관리비)
<br/>

`-` 반도체와 반도체장비 관련 기업들 중 현재 거래량이 많은 기업 40개를 추출하였다.<br/>
(현재 시장 관심과 활발한 거래가 이루어지는 기업 위주)<br/>
<br/>

`-` 등락률>0, 거래량 상위 20인 기업과 등락률<0, 거래량 상위 20인 기업의 데이터프레임 합치기
  
<img width="486" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/ba27bd0e-f877-4312-ba08-6872e50cbedb"><br/>


`-` 영업이익증가율과 매출액증가율의 분포 <br/>
<img width="1160" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/6010738c-de34-4d7c-8ec5-b2c37128fd56"><br/>


```
- 영업이익률도 크게 나쁘지 않고 매출액 증가율도 높은 편이다.
- 대기업의 매출액과 영업이익 감소가 반도체 지표에 큰 영향을 주고 있음을 알 수 있다.
- 올해 하반기에는 계절적 수요 증가와 공급 조절에 따른 재고소진 속도가 빨라질 것이며 연말로 갈수록 글로벌 경제 및 IT 수요 회복에 힘입어서 성장 모멘텀이 개선될 전망이라고 한다. (출처 : 국제금융센터 - 글로벌 반도체 업황 전망 및 국내 영향)
```

<br/>

2) PER, ROE 분석

- PER : 한주의 주가가 얼마인지 나타내는 지표 (주가/주당순이익) => 주당순이익이 같다면 주가가 낮은 기업(저평가기업)이 오를 가능성이 높다.
- ROE : 기업의 자기자본에 대한 수익성을 나타내는 지표 ((당기순이익/자기자본)*100) => ROE가 높으면 주주에게 높은 수익을 창출하고 기업이 효율적으로 자본을 운용한다는 의미.

<img width="894" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/b9ed4cf1-200e-4e10-8d28-0d5e49e8a7d2"><br/>


- 몇몇 기업들을 제외하고 대부분 기업들이 PER과 ROE가 비슷한 수치를 가지고 있다. 기업의 수익성과 가치가 비슷한 수준으로 평가되고 있음을 알 수 있다.(성장가능성과 안정성이 균형을 이루고 있음)

<br/>

## 3. 네이버증권에서 얻을 수 있는 추가적인 통계분석
1) 삼성전자와 sk하이닉스의 외국인 소진율 비교(2016년부터 지금까지 월별 데이터 수집)
<img width="818" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/a7500a32-9655-4787-bde4-4e14c04a1d4d">


- 두 기업 모두 높은 외국인 소진율을 가지고 있다.

2) 삼성전자의 거래량과 종가의 상관관계

<img width="835" alt="image" src="https://github.com/wjdwls23/myrepo/assets/127033471/da0bfe50-108a-4eaa-941c-d3c4c6b91c11">

- 거래량과 종가가 양의 상관관계를 가짐을 알 수 있다. 회귀선의 기울기가 유의미한지 t-검정을 한 결과 p-value가 기각되어 유의미하다고 할 수 있다.


  ## <결론>
  > 현재 반도체 1분기 기업성장과 수익 및 안정성이 악화에는 상위 대기업의 영업손실 영향이 큰 것을 알 수 있었다. <br/>
  > 또한 글로벌 반도체 업황이 우리나라 수출에 미치는 영향력이 큰 상황에서 하반기부터는 국내 반도체 수출 회복이 예상되나 지정학적 위험 확대도 잠재되어있다. <br/>
  (지정학적 위험 -> 우리나라 본도체 수출의 약 40%를 중국이 책임지고 있는데 미국이 대중국 규제를 지속하고 반도체 국산화를 강하게 추진하고 있다.) <br/>
  > 최근에는 챗gpt와 같은 인공지능 기술이 빠르게 발전하면서 반도체 수요는 계속 증가할 것으로 보인다.
   



