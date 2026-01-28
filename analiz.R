library(readxl)
library(tidyverse)
library(lubridate)

# 1. Dosyayı seç
df <- read_excel(file.choose())

# 2. Tarihleri temizleme (Agresif Mod)
df_clean <- df %>%
  mutate(
    # orders kısmında dmy (gün-ay-yıl) ve ymd (yıl-ay-gün) gibi tüm varyasyonları denetiyoruz
    Publish_time = parse_date_time(Publish_time, orders = c("dmy HM", "dmy HMS", "ymd HM", "ymd HMS", "d.m.y H:M")),
    Reach = as.numeric(Reach),
    Likes = as.numeric(Likes)
  )

# 3. KONTROL: Hala NA var mı?
sum(is.na(df_clean$Publish_time))
df_analysis <- df_clean %>%
  mutate(
    Period = ifelse(Publish_time >= as.Date("2025-11-11"), "Yeni Strateji", "Eski Strateji")
  )

# Dönemlere göre ortalama etkileşimi görelim
summary_table <- df_analysis %>%
  group_by(Period) %>%
  summarise(
    Ortalama_Erisim = mean(Reach, na.rm = TRUE),
    Ortalama_Begeni = mean(Likes, na.rm = TRUE),
    Paylasim_Sayisi = n()
  )

print(summary_table)

ggplot(df_analysis, aes(x = Period, y = Likes, fill = Period)) +
  geom_boxplot(alpha=0.7) + 
  scale_fill_manual(values = c("Eski Strateji" = "#E69F00" , "Yeni Strateji" = "#56B4E9"))+
  labs(
    title = "@ilkbahartonu Strateji Karşılaştırması",
    subtitle = "11 Kasım Öncesi ve Sonsrası Beğeni Dağılımı",
    x = "Dönem",
    y = "Beğeni Sayısı"
  ) + 
  theme_minimal()

# Video süresi ile Beğeni arasındaki ilişki (Scatter Plot)
ggplot(df_analysis %>% filter(Type == "IG Reels"), aes(x = Duration, y = Likes)) +
  geom_point(color = "#56B4E9", size = 3, alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + # Regresyon çizgisi
  labs(
    title = "Video Süresi vs Beğeni Analizi",
    x = "Süresi (Saniye)",
    y = "Beğeni Sayısı"
  ) +
  theme_minimal()


# Haftalık paylaşım yoğunluğu ve beğeni trendi
df_analysis %>%
  mutate(Week = floor_date(Publish_time, "week")) %>%
  group_by(Week) %>%
  summarise(Paylasim_Sayisi = n(), Ort_Begeni = mean(Likes)) %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = Paylasim_Sayisi * 10, color = "Paylaşım Sıklığı (x10)"), size = 1) +
  geom_line(aes(y = Ort_Begeni, color = "Ortalama Beğeni"), size = 1) +
  labs(title = "@ilkbahartonu Haftalık Performans Trendi",
       x = "Zaman", y = "Değer") +
  theme_minimal()




# Sadece yeni strateji dönemini filtreleyip saatleri çıkarıyoruz
df_new_hours <- df_analysis %>%
  filter(Period == "Yeni Strateji") %>%
  mutate(Hour = hour(Publish_time))

ggplot(df_new_hours, aes(x = factor(Hour), y = Likes, fill = factor(Hour))) +
  geom_bar(stat = "summary", fun = "mean", fill = "#56B4E9") +
  labs(title = "@ilkbahartonu: Yeni Strateji Saatlik Etkileşim",
       subtitle = "11 Kasım Sonrası En Verimli Saatler",
       x = "Paylaşım Saati (24s)", y = "Ortalama Beğeni") +
  theme_minimal()




# Sadece yeni dönem ve gün analizi
df_new_days <- df_analysis %>%
  filter(Period == "Yeni Strateji") %>%
  mutate(Day = wday(Publish_time, label = TRUE, abbr = FALSE, locale = "Turkish"))

ggplot(df_new_days, aes(x = Day, y = Reach, fill = Day)) +
  geom_bar(stat = "summary", fun = "mean", fill = "#E69F00") +
  labs(title = "@ilkbahartonu: Yeni Strateji Günlük Erişim",
       subtitle = "11 Kasım Sonrası En İyi Performans Gösteren Günler",
       x = "Günler", y = "Ortalama Erişim") +
  theme_minimal() + theme(legend.position = "none")


# Haftanın günlerini Türkçeleştirip sıralayalım
df_days <- df_analysis %>%
  mutate(Day = wday(Publish_time, label = TRUE, abbr = FALSE, locale = "Turkish"))

ggplot(df_days, aes(x = Day, y = Reach, fill = Day)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "@ilkbahartonu Haftalık Erişim Analizi",
       x = "Günler", y = "Ortalama Erişim") +
  theme_minimal() + theme(legend.position = "none")

#strateji bazlı etkileşim oranı
df_eng <- df_analysis %>%
  mutate(Eng_Rate = (Likes + Comments + Saves) / Reach * 100)

ggplot(df_eng, aes(x = Period, y = Eng_Rate, fill = Period)) +
  geom_violin(alpha = 0.5) + # Dağılımın şeklini görmek için violin plot
  labs(title = "Strateji Bazlı Etkileşim Oranı (%)",
       y = "Etkileşim Oranı", x = "") +
  theme_minimal()
#içerik türüne göre kaydedilme sayıları
ggplot(df_analysis, aes(x = Type, y = Saves, fill = Type)) +
  geom_boxplot() +
  labs(title = "İçerik Türüne Göre Kaydedilme Sayıları",
       x = "İçerik Türü", y = "Kaydedilme") +
  theme_minimal()

# Yeni strateji döneminde en çok beğeni alan ilk 5 post
top_5_new <- df_analysis %>%
  filter(Period == "Yeni Strateji") %>%
  arrange(desc(Likes)) %>%
  slice(1:5) %>%
  select(Publish_time, Type, Likes, Reach, Saves)

print(top_5_new)


# Sadece yeni strateji dönemini (11 Kasım sonrası) alıyoruz
df_new_era <- df_analysis %>%
  filter(Period == "Yeni Strateji") %>%
  mutate(Hour = hour(Publish_time))

# Saatlik ortalama beğeni grafiği
ggplot(df_new_era, aes(x = factor(Hour), y = Likes, fill = factor(Hour))) +
  geom_bar(stat = "summary", fun = "mean", fill = "#56B4E9") +
  labs(title = "@ilkbahartonu: Yeni Strateji Saatlik Etkileşim",
       subtitle = "11 Kasım Sonrası Algoritma Yayılım Analizi",
       x = "Günün Saatleri (24s)", 
       y = "Ortalama Beğeni") +
  theme_minimal() +
  theme(legend.position = "none")

