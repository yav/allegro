BUILD:=build

$(BUILD)/%: %.c
	mkdir -p $(BUILD)
	gcc -std=c99 -Wall $^ -o $@ -lallegro -lallegro_image

.PHONY: clean
clean:
	rm -rf $(BUILD)
