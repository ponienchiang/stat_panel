        var bisectX = d3.bisector(function(d) {
            return d.x;
        }).left;
        var pct = d3.format('02.2f');
        var interval = 0.05,
            upper_bound = 3.1,
            lower_bound = -3.0,
            mean = 0,
            std = 1;
        // width/height padding for transparent rect
        var rect_width = 20,
            line_offset = 0.5; //offset so vertical lines go with ticks
        var margin = {
            top: 50,
            right: 20,
            bottom: 50,
            left: 50
        };

        var width = 960 - margin.left - margin.right,
            height = 500 - margin.top - margin.bottom;

        var svg = d3.select("#chartdiv").append("svg")
            .attr("width", width + margin.left + margin.right)
            .attr("height", height + margin.top + margin.bottom)
            .append("g")
            .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

        var dataset = create_data(interval, upper_bound, lower_bound, mean, std);
        // data point where two selectable lines will be initialized
        var right_index = bisectX(dataset, 1.0),
            left_index = bisectX(dataset, -1.0);

        //Define Scales
        var xScale = d3.scaleLinear()
            .domain([d3.min(dataset, function(d) {
                return d.x;
            }), d3.max(dataset, function(d) {
                return d.x;
            })])
            .rangeRound([0, width])
            .clamp(true);

        var yScale = d3.scaleLinear()
            .domain([
                d3.min(dataset, function(d) {
                    return (d.y);
                }),
                d3.max(dataset, function(d) {
                    return d.y;
                })
            ])
            .range([height, 0]);

        var area = d3.area()
            .x(function(d) {
                return xScale(d.x);
            })
            .y1(function(d) {
                return yScale(d.y);
            });


        var xlabels = ['-3\u03C3', '-2\u03C3', '-\u03C3',
            '0', '\u03C3', '2\u03C3', '3\u03C3'
        ];

        /////// Define Axis //////////////////////////////
        var xAxis = d3.axisBottom()
            .scale(xScale)
            .ticks(xlabels.length)
            .tickFormat(function(d, i) {
                return xlabels[i];
            });

        var yAxis = d3.axisLeft()
            .scale(yScale)
            .ticks(8);

        // append distribution points
        svg.append("g")
            .attr("id", "circles")
            .selectAll("circle")
            .data(dataset)
            .enter()
            .append("circle")
            .attr("class", "dot")
            .attr("cx", function(d) {
                return xScale(d.x);
            })
            .attr("cy", function(d) {
                return yScale(d.y);
            })
            .attr("r", 3.0)


        // cut off datapoints that are outside the axis
        svg.append("clipPath")
            .attr("id", "chart-area")
            .append("rect")
            .attr("width", width)
            .attr("height", height);

        // set up area
        area.y0(yScale(0));
        svg.append("path")
            .data([dataset.slice(left_index, right_index)])
            .attr("clip-path", "url(#chart-area)")
            .attr("class", "area")
            .attr("fill", "steelblue")
            .attr("d", area);

        // center display
        svg.append("text")
            .attr("id", "pdisplay")
            .attr("x", xScale(0))
            .attr("y", yScale(0.2))
            .style("text-anchor", "middle");


        // create lines (focuses)
        var focus_a = create_focus(left_index);
        var focus_b = create_focus(right_index);
        // set up center display
        redraw_area();
        // append Axes
        svg.append("g")
            .attr("class", "x axis")
            .attr("transform", "translate(0," + height + ")")
            .call(xAxis);

        svg.append("g")
            .attr("class", "y axis")
            .call(yAxis)
            .append("text")
            .attr("transform", "rotate(-90)")
            .attr("y", 6)
            .attr("x", -10)
            .attr("dy", "0.71em")
            .attr("fill", "#000")
            .text("Probability Density");;

        function create_focus(start_point) {
            var focus = svg.append("g")
                .attr("class", "focus")
                .style("display", "inline")
                .call(d3.drag()
                    .on("drag", dragging));

            focus.append("circle")
                .attr("r", 4.5);

            //  Set up focus (container for vertical guiding line)
            focus.attr("transform", "translate(" + xScale(dataset[start_point].x) +
                "," + yScale(dataset[start_point].y) + ")");

            focus.append("line")
                .attr('x1', line_offset)
                .attr('x2', line_offset)
                .attr('y1', 0)
                .attr('y2', height - yScale(dataset[start_point].y));

            // transparent rect to allow for skinny lines to be dragged
            // easily (rect wider than the line)
            focus.append("rect")
                .attr("x", -rect_width / 2)
                .attr("y", -rect_width)
                .attr("width", rect_width)
                .attr("height", height - yScale(dataset[start_point].y) + rect_width);

            return focus;
        }

        function dragging() {
            // identify drag position
            d = get_data_point(d3.event.x)
                // update line and rect of focus
            d3.select(this).attr("transform", "translate(" + xScale(d.x) + "," + yScale(d.y) + ")");
            d3.select(this).select('line')
                .attr('x1', line_offset)
                .attr('x2', line_offset)
                .attr('y1', 0)
                .attr('y2', height - yScale(d.y));
            d3.select(this).select("rect")
                .attr("height", height - yScale(d.y) + rect_width);

            redraw_area();


        }

        function get_coordinates(transform_string) {
            // parse translate("x", "y") values
            var parsed_coord = /translate\(\s*([^\s,)]+)[ ,]([^\s,)]+)/.exec(transform_string);
            var x = +parsed_coord[1],
                y = +parsed_coord[2];
            return [x, y];
        }

        function get_data_point(data_point) {
            var x0 = xScale.invert(data_point),
                i = bisectX(dataset, x0);
            if (i == 0) {
                return dataset[i];
            }
            var d0 = dataset[i - 1],
                d1 = dataset[i];
            return x0 - d0.x > d1.x - x0 ? d1 : d0;

        }

        function redraw_area() {
            // get coordinate values for each of the focus elements
            a_transform = get_coordinates(focus_a.attr("transform"));
            b_transform = get_coordinates(focus_b.attr("transform"));

            a = get_data_point(a_transform[0]);
            b = get_data_point(b_transform[0]);
            // create an array of sorted data points
            area_range = [a, b].sort(function(a, b) {
                return a.x - b.x;
            });

            // update area
            svg.select("path")
                .data([dataset.slice(dataset.indexOf(area_range[0]), dataset.indexOf(area_range[1]) + 1)])
                .attr("d", area);
            // Update center display
            svg.select("#pdisplay").text('p(x\u2081 \u2264 X \u2264 x\u2082) = ' +
                pct(100 * (jStat.normal.cdf(area_range[1].x, mean, std) -
                    jStat.normal.cdf(area_range[0].x, mean, std))) + "%"
            );
        }

        function create_data(interval, upper_bound, lower_bound, mean, std) {
            var n = Math.floor((upper_bound - lower_bound) / interval)
            var data = [];

            x_position = lower_bound;
            for (i = 0; i < n; i++) {
                data.push({
                    "y": jStat.normal.pdf(x_position, mean, std),
                    "x": x_position
                })
                x_position = Math.round((interval + x_position) * 100) / 100

            }
            return (data);
        }