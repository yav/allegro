BUILD:=build

$(BUILD)/%: %.c
	mkdir -p $(BUILD)
	gcc -std=c99 -Wall $^ -o $@ \
          -lallegro \
          -lallegro_image \
          -lallegro_font \
          -lallegro_ttf \
          -lallegro_audio \
          -lallegro_acodec \
          -lm

.PHONY: clean
clean:
	rm -rf $(BUILD)
